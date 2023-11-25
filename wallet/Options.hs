{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options (Cmd (..), Network (..), Options (..), parseOptions) where

import Prelude hiding (id)

import Control.Applicative ((<**>), (<|>))
import Data.Foldable (fold)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Sql (toSqlKey)
import Options.Applicative (ParserInfo, argument, auto, command,
                            customExecParser, disambiguate, flag, fullDesc,
                            help, helper, hsubparser, info, internal, long,
                            metavar, prefs, progDesc, showHelpOnError,
                            strArgument, strOption, subparserInline, switch,
                            value)
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import System.FilePath ((</>))

import Accounts (Account, AccountId)

appName :: String
appName = "wallet"

data Network = TestNet | PublicNet
    deriving (Show)

data Options = Options
    -- global options
    { database :: FilePath
    , network :: Network
    -- subcommand
    , cmd :: Cmd
    }
    deriving (Show)

data Cmd
    = AddAccount{{- argument -} publicKey :: Text}
    | ListAccounts
    | ShowAccount{{- argument -} id :: AccountId, {- option -} refresh :: Bool}
    deriving (Show)

parser :: FilePath -> ParserInfo Options
parser defaultDatabasePath =
    (pOptions <**> helper)
    `info` (fullDesc <> progDesc "Simple CLI Stellar wallet")
  where

    pOptions = do
        database    <- pDatabase
        network     <- pNetwork
        cmd         <- pCmd
        pure Options{..}

    pDatabase =
        strOption $
            long "database" <> metavar "FILE" <> value defaultDatabasePath

    pNetwork =
        flag TestNet PublicNet $
        help "Use public network. Default: test network" <> long "public-net"

    pCmd =
        hsubparser (fold pCmds)
        <|> hsubparser (internal <> fold pCmdSynonyms)
        <|> pure ListAccounts

    pCmds =
        [ c "add-account"   "Add an account"        pAddAccount
        , c "list-accounts" "List accounts"         pListAccounts
        , c "show-account"  "Show account details"  pShowAccount
        ]
      where
        c name desc p = command name $ p `info` progDesc desc

    pCmdSynonyms =
        [ c "account-add"   pAddAccount
        , c "account-list"  pListAccounts
        , c "account-show"  pShowAccount
        , c "accounts"      pListAccounts
        , c "accounts-list" pListAccounts
        , c "add"           pAddAccount
        , c "list"          pListAccounts
        , c "list-account"  pListAccounts
        , c "show"          pShowAccount
        ]
      where
        c name p = command name $ p `info` mempty

    argPublicKey =
        strArgument $ help "Public key (address)" <> metavar "PUBLIC_KEY"

    pAddAccount = do
        publicKey <- argPublicKey
        pure AddAccount{..}

    argAccountId =
        argument (toSqlKey @Account <$> auto @Int64) $ metavar "ACCOUNT_ID"

    pShowAccount = do
        id      <- argAccountId
        refresh <-
            switch $
            help "Refresh account data from the network" <> long "refresh"
        pure ShowAccount{..}

    pListAccounts = pure ListAccounts

parseOptions :: IO Options
parseOptions = do
    appDataDir <- getXdgDirectory XdgData appName
    let defaultDatabasePath = appDataDir </> "state.sqlite"
    customExecParser myprefs $ parser defaultDatabasePath
  where
    myprefs = prefs $ disambiguate <> showHelpOnError <> subparserInline
