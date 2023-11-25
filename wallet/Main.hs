{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

import Prelude hiding (id)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.Text qualified as Text
import Database.Persist (Entity (..))
import Database.Persist.Sql (SqlPersistM, fromSqlKey, runMigration)
import Database.Persist.Sqlite (runSqlite)
import Named (NamedF (Arg), (:!))
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client (mkClientEnv)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

import Stellar.Horizon.Client qualified as StellarNet
import Stellar.Simple (runClientThrow)

import Accounts (Account (..))
import Accounts qualified
import Options (Cmd (..), Network (..), Options (..), parseOptions)

main :: IO ()
main = do
    Options{database, network, cmd} <- parseOptions
    manager <- newTlsManager
    let stellarServer =
            case network of
                TestNet     -> StellarNet.testServerBase
                PublicNet   -> StellarNet.publicServerBase
    let stellarClient = mkClientEnv manager stellarServer
    case cmd of
        AddAccount{publicKey} -> do
            accountId <- withDatabaseReadWrite database $ Accounts.add publicKey
            putStrLn $ "Added account " <> show (fromSqlKey accountId)
        ListAccounts -> do
            accounts <-
                withDatabaseReadOnly database Accounts.getAll $ #fallback []
            case accounts of
                [] -> putStrLn "No accounts"
                _ -> for_ accounts print
        ShowAccount{id, refresh}
            | refresh ->
                withDatabaseReadWrite database do
                    mAccount <- Accounts.get id
                    dbAccount <-
                        case mAccount of
                            Just account -> pure account
                            Nothing -> die "No such account"
                    let Account{publickey} = dbAccount
                    netAccount <-
                        liftIO $
                        (`runClientThrow` stellarClient) $
                        StellarNet.getAccount $ StellarNet.Address publickey
                    Accounts.saveFromNet (Entity id dbAccount) netAccount
                    die "not implemented: printing result"
            | otherwise -> do
                mAccount <-
                    withDatabaseReadOnly
                        database
                        (Accounts.get id)
                        (#fallback Nothing)
                case mAccount of
                    Just account -> do
                        putStr $ "Cached data:\n" ++ replicate 4 ' '
                        print account
                    Nothing -> putStrLn "Account is not found"

-- | Database is NOT guaranteed to be read only.
-- It only checks existence before reading.
withDatabaseReadOnly ::
    FilePath ->
    SqlPersistM a ->
    -- | Value to return when the database doesn't exist
    "fallback" :! a ->
    IO a
withDatabaseReadOnly database dbaction (Arg fallbackValue) = do
    databaseExists <- doesFileExist database
    if databaseExists then
        runSqlite (Text.pack database) dbaction
    else
        pure fallbackValue

withDatabaseReadWrite :: FilePath -> SqlPersistM a -> IO a
withDatabaseReadWrite database dbaction = do
    createDirectoryIfMissing {- create parents -}True $ takeDirectory database
    runSqlite (Text.pack database) do
        runMigration Accounts.migrateAll
        dbaction

die :: MonadIO io => String -> io a
die err = liftIO $ hPutStrLn stderr err >> exitFailure
