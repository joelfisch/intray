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

import Data.Aeson as JSON
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Map as M
import Data.Map (Map)
import Data.Mergeless
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed
import Import
import Intray.API.Types ()
import Intray.Data
import Servant.Docs

data TypedItem =
  TypedItem
    { itemType :: ItemType
    , itemData :: ByteString
    }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity TypedItem

instance FromJSON TypedItem where
  parseJSON =
    withObject "TypedItem" $ \o ->
      TypedItem <$> o .: "type" <*>
      (do t <- o .: "data"
          case Base64.decode $ SB8.pack t of
            Left err -> fail $ unwords ["Failed to decode base64-encoded typed item data:", err]
            Right r -> pure r)

instance ToJSON TypedItem where
  toJSON TypedItem {..} = object ["type" .= itemType, "data" .= SB8.unpack (Base64.encode itemData)]

instance ToSample TypedItem where
  toSamples Proxy = singleSample $ TypedItem TextItem "Hello World!"

textTypedItem :: Text -> TypedItem
textTypedItem t = TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}

typedItemCase :: TypedItem -> Either String TypedItemCase
typedItemCase TypedItem {..} =
  case itemType of
    TextItem -> left show $ CaseTextItem <$> TE.decodeUtf8' itemData
    ImageItem it -> pure $ CaseImageItem it itemData

data TypedItemCase
  = CaseTextItem Text
  | CaseImageItem ImageType ByteString
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

instance ToSample a => ToSample (ItemInfo a)

instance (Ord ci, ToSample ci, Ord si, ToSample si, ToSample a) =>
         ToSample (SyncRequest ci si a)

instance (Ord ci, ToSample ci, Ord si, ToSample si, ToSample a) =>
         ToSample (SyncResponse ci si a)

instance (Ord k, ToSample k, ToSample v) => ToSample (Map k v) where
  toSamples Proxy = fmapSamples M.fromList $ toSamples Proxy

fmapSamples :: (a -> b) -> [(Text, a)] -> [(Text, b)]
fmapSamples f = map (second f)
