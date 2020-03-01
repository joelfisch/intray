{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.API.Protected.AccessKey.Types
  ( AccessKeyInfo(..)
  , AccessKeyUUID
  , AddAccessKey(..)
  , AccessKeyCreated(..)
  , module Data.UUID.Typed
  ) where

import Import

import Data.Aeson as JSON
import Data.Set (Set)
import Data.Swagger
import Data.Time
import Data.UUID.Typed

import Servant.Docs

import Intray.Data

import Intray.API.Types ()

data AccessKeyInfo =
  AccessKeyInfo
    { accessKeyInfoUUID :: AccessKeyUUID
    , accessKeyInfoName :: Text
    , accessKeyInfoCreatedTimestamp :: UTCTime
    , accessKeyInfoPermissions :: Set Permission
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity AccessKeyInfo

instance FromJSON AccessKeyInfo where
  parseJSON =
    withObject "AccessKeyInfo" $ \o ->
      AccessKeyInfo <$> o .: "uuid" <*> o .: "name" <*> o .: "created" <*> o .: "permissions"

instance ToJSON AccessKeyInfo where
  toJSON AccessKeyInfo {..} =
    object
      [ "uuid" .= accessKeyInfoUUID
      , "name" .= accessKeyInfoName
      , "created" .= accessKeyInfoCreatedTimestamp
      , "permissions" .= accessKeyInfoPermissions
      ]

instance ToSample AccessKeyInfo

data AddAccessKey =
  AddAccessKey
    { addAccessKeyName :: Text
    , addAccessKeyPermissions :: Set Permission
    }
  deriving (Show, Eq, Generic)

instance Validity AddAccessKey

instance FromJSON AddAccessKey

instance ToJSON AddAccessKey

instance ToSample AddAccessKey

instance ToSchema AddAccessKey

data AccessKeyCreated =
  AccessKeyCreated
    { accessKeyCreatedCreatedTimestamp :: UTCTime
    , accessKeyCreatedKey :: AccessKeySecret
    , accessKeyCreatedUUID :: AccessKeyUUID
    }
  deriving (Show, Eq, Generic)

instance Validity AccessKeyCreated

instance FromJSON AccessKeyCreated

instance ToJSON AccessKeyCreated

instance ToSample AccessKeyCreated

instance ToSchema AccessKeyCreated
