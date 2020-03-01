{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Item.Types
  ( ItemType(..)
  , TypedItem(..)
  , textTypedItem
  , TypedItemCase(..)
  , typedItemCase
  , AddedItem(..)
  , ItemInfo(..)
  , ItemUUID
  , module Data.UUID.Typed
  ) where

import Import

import Data.Aeson as JSON
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import Data.Function
import qualified Data.Map as M
import Data.Map (Map)
import Data.Mergeless
import qualified Data.Set as S
import Data.Set (Set)
import Data.Swagger
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed
import Lens.Micro

import Servant.Docs
import Servant.Swagger

import Intray.Data

import Intray.API.Types ()

data TypedItem =
  TypedItem
    { itemType :: ItemType
    , itemData :: Text
    }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity TypedItem

instance FromJSON TypedItem where
  parseJSON = withObject "TypedItem" $ \o -> TypedItem <$> o .: "type" <*> o .: "data"

instance ToJSON TypedItem where
  toJSON TypedItem {..} = object ["type" .= itemType, "data" .= itemData]

instance ToSample TypedItem where
  toSamples Proxy = singleSample $ TypedItem TextItem "Hello World!"

instance ToSchema ItemType

instance ToSchema TypedItem

textTypedItem :: Text -> TypedItem
textTypedItem t = TypedItem {itemType = TextItem, itemData = t}

typedItemCase :: TypedItem -> TypedItemCase
typedItemCase TypedItem {..} =
  case itemType of
    TextItem -> CaseTextItem itemData

newtype TypedItemCase =
  CaseTextItem Text
  deriving (Show, Read, Eq, Ord, Generic)

data AddedItem a =
  AddedItem
    { addedItemContents :: a
    , addedItemCreated :: UTCTime
    }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (AddedItem a)

instance ToJSON a => ToJSON (AddedItem a) where
  toJSON AddedItem {..} = object ["contents" .= addedItemContents, "created" .= addedItemCreated]

instance FromJSON a => FromJSON (AddedItem a) where
  parseJSON = withObject "AddedItem" $ \o -> AddedItem <$> o .: "contents" <*> o .: "created"

instance (ToSample a) => ToSample (AddedItem a)

instance (ToSchema a) => ToSchema (AddedItem a)

data ItemInfo a =
  ItemInfo
    { itemInfoIdentifier :: ItemUUID
    , itemInfoContents :: a
    , itemInfoCreated :: UTCTime
    }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (ItemInfo a)

instance ToJSON a => ToJSON (ItemInfo a) where
  toJSON ItemInfo {..} =
    object
      ["id" .= itemInfoIdentifier, "contents" .= itemInfoContents, "created" .= itemInfoCreated]

instance FromJSON a => FromJSON (ItemInfo a) where
  parseJSON =
    withObject "ItemInfo TypedItem" $ \o ->
      ItemInfo <$> o .: "id" <*> o .: "contents" <*> o .: "created"

instance ToSample ClientId where
  toSamples Proxy = singleSample (ClientId 0)

instance ToParamSchema ClientId

instance ToSchema ClientId

instance ToSample a => ToSample (ItemInfo a)

instance ToSchema a => ToSchema (ItemInfo a)

instance (Ord i, ToSample i, ToSample a) => ToSample (SyncRequest i a)

instance (Ord i, ToJSONKey i, ToSchema i, ToSchema a) => ToSchema (SyncRequest i a)

instance (Ord i, ToSample i, ToSample a) => ToSample (SyncResponse i a)

instance (Ord i, ToJSONKey i, ToSchema i, ToSchema a) => ToSchema (SyncResponse i a)

instance (Ord k, ToSample k, ToSample v) => ToSample (Map k v) where
  toSamples Proxy = fmapSamples M.fromList $ toSamples Proxy

fmapSamples :: (a -> b) -> [(Text, a)] -> [(Text, b)]
fmapSamples f = map (second f)
