{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.API.Protected.Account.Types
  ( module Intray.API.Protected.Account.Types
  , module Data.UUID.Typed
  ) where

import Data.Aeson as JSON
import Data.Time
import Data.UUID.Typed
import Import
import Intray.API.Types ()
import Intray.Data
import Servant.Docs

data AccountInfo =
  AccountInfo
    { accountInfoUUID :: AccountUUID
    , accountInfoUsername :: Username
    , accountInfoCreatedTimestamp :: UTCTime
    , accountInfoLastLogin :: Maybe UTCTime
    , accountInfoAdmin :: Bool
    , accountInfoCount :: Int
    , accountInfoStatus :: PaidStatus
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity AccountInfo

instance FromJSON AccountInfo where
  parseJSON =
    withObject "AccountInfo" $ \o ->
      AccountInfo <$> o .: "uuid" <*> o .: "username" <*> o .: "created" <*> o .: "last-login" <*>
      o .: "admin" <*>
      o .: "count" <*>
      o .: "status"

instance ToJSON AccountInfo where
  toJSON AccountInfo {..} =
    object
      [ "uuid" .= accountInfoUUID
      , "username" .= accountInfoUsername
      , "created" .= accountInfoCreatedTimestamp
      , "last-login" .= accountInfoLastLogin
      , "admin" .= accountInfoAdmin
      , "count" .= accountInfoCount
      , "status" .= accountInfoStatus
      ]

instance ToSample AccountInfo

data PaidStatus
  = HasNotPaid Int -- Number of extra items that they're still allowed
  | HasPaid UTCTime
  | NoPaymentNecessary
  deriving (Show, Eq, Ord, Generic)

instance Validity PaidStatus

instance FromJSON PaidStatus where
  parseJSON =
    withObject "PaidStatus" $ \o -> do
      t <- o .: "status"
      case (t :: Text) of
        "not-paid" -> HasNotPaid <$> o .: "items-left"
        "paid" -> HasPaid <$> o .: "until"
        "no-payment-necessary" -> pure NoPaymentNecessary
        _ -> fail "Unknown PaidStatus"

instance ToJSON PaidStatus where
  toJSON =
    let o t vs = object $ ("status" .= (t :: Text)) : vs
     in \case
          HasNotPaid itemsLeft -> o "not-paid" ["items-left" .= itemsLeft]
          HasPaid ut -> o "paid" ["until" .= ut]
          NoPaymentNecessary -> o "no-payment-necessary" []

instance ToSample PaidStatus

data ChangePassphrase =
  ChangePassphrase
    { changePassphraseOld :: Text
    , changePassphraseNew :: Text
    }
  deriving (Show, Eq, Generic)

instance Validity ChangePassphrase

instance FromJSON ChangePassphrase where
  parseJSON =
    withObject "ChangePassphrase" $ \o ->
      ChangePassphrase <$> o .: "old-passphrase" <*> o .: "new-passphrase"

instance ToJSON ChangePassphrase where
  toJSON ChangePassphrase {..} =
    object ["old-passphrase" .= changePassphraseOld, "new-passphrase" .= changePassphraseNew]

instance ToSample ChangePassphrase
