{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))
import Data.Text (Text)
import Options.Generic (Generic, ParseRecord, getRecord)
import Stellar.Simple (
    Address (Address),
    Asset,
    op_changeTrust,
    transactionBuilder,
 )

data Options = Trust {account :: Text, asset :: Text} | Pay
    deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
    options <- getRecord "Simple CLI Stellar wallet"
    exec options

exec :: Options -> IO ()
exec = \case
    Trust{account, asset} -> execTrust (Address account) (_ asset)
    Pay -> undefined

execTrust :: Address -> Asset -> IO ()
execTrust account asset = do
    let txb = transactionBuilder account & op_changeTrust asset
    _
