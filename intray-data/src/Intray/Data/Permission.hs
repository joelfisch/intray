{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Data.Permission where

import Intray.Data.Import

import Data.Aeson
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import Text.Read

import Database.Persist
import Database.Persist.Sql

data Permission
  = PermitAdd
  | PermitShow
  | PermitSize
  | PermitDelete
  | PermitGetItem
  | PermitGetItems
  | PermitGetItemUUIDs
  | PermitSync
  | PermitDeleteAccount
  | PermitGetAccountInfo
  | PermitPostChangePassphrase
  | PermitPostAddAccessKey
  | PermitGetAccessKey
  | PermitGetAccessKeys
  | PermitDeleteAccessKey
  | PermitGetPermissions
  | PermitAdminDeleteAccount
  | PermitAdminGetAccounts
  | PermitAdminGetStats
  deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded)

instance Validity Permission

instance FromJSON Permission

instance ToJSON Permission

instance PersistField Permission where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue pv = do
    t <- fromPersistValueText pv
    case readMaybe $ T.unpack t of
      Nothing -> Left "Unknown Permission value."
      Just p -> pure p

instance PersistFieldSql Permission where
  sqlType Proxy = SqlString

userPermissions :: Set Permission
userPermissions =
  S.fromList
    [ PermitAdd
    , PermitShow
    , PermitSize
    , PermitDelete
    , PermitGetItem
    , PermitGetItems
    , PermitGetItemUUIDs
    , PermitSync
    , PermitDeleteAccount
    , PermitGetAccountInfo
    , PermitPostChangePassphrase
    , PermitPostAddAccessKey
    , PermitGetAccessKey
    , PermitGetAccessKeys
    , PermitDeleteAccessKey
    , PermitGetPermissions
    ]

adminOnlyPermissions :: Set Permission
adminOnlyPermissions =
  S.fromList [PermitAdminDeleteAccount, PermitAdminGetAccounts, PermitAdminGetStats]

adminPermissions :: Set Permission
adminPermissions = S.union userPermissions adminOnlyPermissions

allPermissions :: Set Permission
allPermissions = S.fromList [minBound .. maxBound]
