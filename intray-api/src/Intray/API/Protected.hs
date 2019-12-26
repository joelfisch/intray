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
import Servant.Generic

import Data.Set (Set)

import Intray.Data

import Intray.API.Protected.AccessKey
import Intray.API.Protected.Account
import Intray.API.Protected.Item
import Intray.API.Types

type IntrayProtectedAPI = ToServant (IntrayProtectedSite AsApi)

data IntrayProtectedSite route = IntrayProtectedSite
    { protectedItemSite :: !(route :- "intray" :> ToServant (IntrayProtectedItemSite AsApi))
    , protectedAccountSite :: !(route :- "account" :> ToServant (IntrayProtectedAccountSite AsApi))
    , protectedAccessKeySite :: !(route :- "access-key" :> ToServant (IntrayProtectedAccessKeySite AsApi))
    , getPermissions :: !(route :- GetPermissions)
    } deriving (Generic)

type GetPermissions
     = ProtectAPI :> "permissions" :> Get '[ JSON] (Set Permission)
