{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected
  ( IntrayProtectedAPI
  , IntrayProtectedSite(..)
  , IntrayProtectedItemAPI
  , IntrayProtectedItemSite(..)
  , IntrayProtectedAccountAPI
  , IntrayProtectedAccountSite(..)
  , IntrayProtectedAccessKeyAPI
  , IntrayProtectedAccessKeySite(..)
  , AuthCookie(..)
  , GetItemUUIDs
  , GetItems
  , GetShowItem
  , GetIntraySize
  , PostAddItem
  , GetItem
  , DeleteItem
  , ItemType(..)
  , TypedItem(..)
  , textTypedItem
  , TypedItemCase(..)
  , typedItemCase
  , ItemInfo(..)
  , Added(..)
  , Synced(..)
  , SyncRequest(..)
  , SyncResponse(..)
  , PostSync
  , AccountInfo(..)
  , GetAccountInfo
  , DeleteAccount
  , AccessKeyUUID
  , AccessKeyInfo(..)
  , AddAccessKey(..)
  , AccessKeyCreated(..)
  , PostAddAccessKey
  , GetAccessKey
  , GetAccessKeys
  , DeleteAccessKey
  , ItemUUID
  , AccountUUID
  , Username
  , parseUsername
  , parseUsernameWithError
  , usernameText
  ) where

import Import

import Servant.API
import Servant.API.Generic

import Data.Set (Set)

import Intray.Data

import Intray.API.Protected.AccessKey
import Intray.API.Protected.Account
import Intray.API.Protected.Item
import Intray.API.Types

type IntrayProtectedAPI = ToServantApi IntrayProtectedSite

data IntrayProtectedSite route =
  IntrayProtectedSite
    { protectedItemSite :: !(route :- "intray" :> ToServantApi IntrayProtectedItemSite)
    , protectedAccountSite :: !(route :- "account" :> ToServantApi IntrayProtectedAccountSite)
    , protectedAccessKeySite :: !(route :- "access-key" :> ToServantApi IntrayProtectedAccessKeySite)
    , getPermissions :: !(route :- GetPermissions)
    }
  deriving (Generic)

type GetPermissions = ProtectAPI :> "permissions" :> Get '[ JSON] (Set Permission)
