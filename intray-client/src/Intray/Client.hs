{-# LANGUAGE DataKinds #-}

module Intray.Client
  ( clientGetShowItem
  , clientGetSize
  , clientGetItemUUIDs
  , clientGetItems
  , clientPostAddItem
  , clientGetItem
  , clientDeleteItem
  , clientPostSync
  , clientPostLogin
  , clientGetDocs
  , clientGetPricing
  , clientGetAccountInfo
  , clientDeleteAccount
  , clientPostAddAccessKey
  , clientGetAccessKey
  , clientGetAccessKeys
  , clientDeleteAccessKey
  , clientGetPermissions
  , clientPostRegister
  , clientAdminGetStats
  , clientAdminDeleteAccount
  , clientAdminGetAccounts
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
  , StoreItem(..)
  , Store(..)
  , makeSyncRequest
  , mergeSyncResponse
  , AccountInfo(..)
  , Permission(..)
  , AddAccessKey(..)
  , accessKeySecretText
  , AccessKeyCreated(..)
  , AccessKeyUUID
  , AccessKeyInfo(..)
  , Registration(..)
  , LoginForm(..)
  , GetDocsResponse(..)
  , Pricing(..)
  , AdminStats(..)
  , ItemUUID
  , AccountUUID
  , Username
  , parseUsername
  , parseUsernameWithError
  , usernameText
  , NoContent(..)
  , Token
  , module Data.UUID.Typed
  ) where

import Import

import Data.Mergeless
import Data.Set (Set)
import qualified Data.UUID.Typed

import Servant.API
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Client

import Intray.API

clientGetShowItem :: Token -> ClientM (Maybe (ItemInfo TypedItem))
clientGetSize :: Token -> ClientM Int
clientGetItemUUIDs :: Token -> ClientM [ItemUUID]
clientGetItems :: Token -> ClientM [ItemInfo TypedItem]
clientPostAddItem :: Token -> TypedItem -> ClientM ItemUUID
clientGetItem :: Token -> ItemUUID -> ClientM (ItemInfo TypedItem)
clientDeleteItem :: Token -> ItemUUID -> ClientM NoContent
clientPostSync ::
     Token -> SyncRequest ItemUUID TypedItem -> ClientM (SyncResponse ItemUUID TypedItem)
clientGetAccountInfo :: Token -> ClientM AccountInfo
clientDeleteAccount :: Token -> ClientM NoContent
clientPostAddAccessKey :: Token -> AddAccessKey -> ClientM AccessKeyCreated
clientGetAccessKey :: Token -> AccessKeyUUID -> ClientM AccessKeyInfo
clientGetAccessKeys :: Token -> ClientM [AccessKeyInfo]
clientDeleteAccessKey :: Token -> AccessKeyUUID -> ClientM NoContent
clientGetPermissions :: Token -> ClientM (Set Permission)
clientPostRegister :: Registration -> ClientM NoContent
clientPostLogin ::
     LoginForm
  -> ClientM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
clientGetDocs :: ClientM GetDocsResponse
clientGetPricing :: ClientM (Maybe Pricing)
clientAdminGetStats :: Token -> ClientM AdminStats
clientAdminDeleteAccount :: Token -> AccountUUID -> ClientM NoContent
clientAdminGetAccounts :: Token -> ClientM [AccountInfo]
clientGetShowItem :<|> clientGetSize :<|> clientGetItemUUIDs :<|> clientGetItems :<|> clientPostAddItem :<|> clientGetItem :<|> clientDeleteItem :<|> clientPostSync :<|> clientGetAccountInfo :<|> clientDeleteAccount :<|> clientPostAddAccessKey :<|> clientGetAccessKey :<|> clientGetAccessKeys :<|> clientDeleteAccessKey :<|> clientGetPermissions :<|> clientPostRegister :<|> clientPostLogin :<|> clientGetDocs :<|> clientGetPricing :<|> clientAdminGetStats :<|> clientAdminDeleteAccount :<|> clientAdminGetAccounts =
  client (flatten intrayAPI)
