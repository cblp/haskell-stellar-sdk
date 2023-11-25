{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Accounts (
    Account (..), AccountId,
    add, get, getAll, getByPublicKey, migrateAll, saveFromNet,
) where

import Control.Monad (unless, when)
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Database.Persist (Entity (..), delete, insertMany_, selectFirst,
                         selectList, update, upsert, (=.), (==.))
import Database.Persist qualified as Persist
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Sql (SqlPersistM)
import Database.Persist.TH (mkMigrate, mkPersist, mpsConstraintLabelModifier,
                            mpsFieldLabelModifier, persistFileWith, share,
                            sqlSettings)

import Stellar.Horizon.Client qualified as StellarNet
import Stellar.Horizon.DTO qualified as StellarNet

$(  let lowerFirst t =
            case Text.uncons t of
                Just (a, b) -> Text.cons (Char.toLower a) b
                Nothing -> t
        mpsConstraintLabelModifier entity field =
            entity <> "_" <> lowerFirst field
        mpsFieldLabelModifier _entity = \case
            "Data"  -> "data_"
            "Type"  -> "type_"
            field   -> lowerFirst field
        settings =
            sqlSettings{mpsConstraintLabelModifier, mpsFieldLabelModifier}
    in
    share
        [mkPersist settings, mkMigrate "migrateAll"]
        $(persistFileWith lowerCaseSettings "models.persistentmodels")
    )

add :: Text -> SqlPersistM AccountId
add publickey = entityKey <$> upsert Account{publickey, xlmBalance = Nothing} []

get :: AccountId -> SqlPersistM (Maybe Account)
get = Persist.get

getAll :: SqlPersistM [Entity Account]
getAll = selectList [] []

getByPublicKey :: Text -> SqlPersistM (Maybe (Entity Account))
getByPublicKey publickey = selectFirst [#publickey ==. publickey] []

saveFromNet :: Entity Account -> StellarNet.Account -> SqlPersistM ()
saveFromNet account StellarNet.Account{balances = newNetBalances} = do
    when (oldXlmBalance /= newXlmBalance) $
        update accountId [#xlmBalance =. newXlmBalance]
    oldBalances <- do
        entities <- selectList [#account ==. accountId] []
        pure $
            Map.fromList
                [ ((assetcode, assetissuer), balance)
                | Entity _ AssetBalance{assetcode, assetissuer, balance} <-
                    entities
                ]
    when (oldBalances /= newBalances) do
        let added = newBalances Map.\\ oldBalances
        let removed = Map.keysSet oldBalances Set.\\ Map.keysSet newBalances
        let changed = Map.intersectionWith const newBalances oldBalances
        unless (null added) $
            insertMany_
                [ AssetBalance
                    {account = accountId, assetcode, assetissuer, balance}
                | ((assetcode, assetissuer), balance) <- Map.assocs added
                ]
        for_ removed \(code, issuer) ->
            delete (AssetBalanceKey accountId code issuer)
        for_ (Map.assocs changed) \((code, issuer), newBalance) ->
            update
                (AssetBalanceKey accountId code issuer)
                [#balance =. newBalance]
  where
    Entity accountId Account{xlmBalance = oldXlmBalance} = account
    newXlmBalance =
        listToMaybe
            [ balance
            | StellarNet.Balance{balance, asset_code = Nothing} <-
                newNetBalances
            ]
    newBalances =
        Map.fromList
            [ ((code, issuer), balance)
            | StellarNet.Balance
                    { asset_code = Just code
                    , asset_issuer = Just issuer
                    , balance
                    } <-
                newNetBalances
            ]
