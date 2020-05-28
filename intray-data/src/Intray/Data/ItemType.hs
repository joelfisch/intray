{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Data.ItemType where

import Control.Arrow
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist
import Database.Persist.Sql
import Intray.Data.Import

data ItemType
  = TextItem
  | ImageItem ImageType
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity ItemType

instance PersistField ItemType where
  toPersistValue = toPersistValue . renderItemType
  fromPersistValue (PersistByteString bs) = do
    t <- left (T.pack . show) $ TE.decodeUtf8' bs
    case parseItemType t of
      Nothing -> Left $ "Unknown item type: " <> t
      Just it -> pure it
  fromPersistValue _ = Left "Not a valid ItemType"

instance PersistFieldSql ItemType where
  sqlType Proxy = SqlString

instance FromJSON ItemType where
  parseJSON =
    withText "ItemType" $ \t ->
      case parseItemType t of
        Nothing -> fail $ "Unknown item type: " <> T.unpack t
        Just it -> pure it

instance ToJSON ItemType where
  toJSON = toJSON . renderItemType

renderItemType :: ItemType -> Text
renderItemType =
  \case
    TextItem -> "text"
    ImageItem it -> renderImageType it

parseItemType :: Text -> Maybe ItemType
parseItemType =
  \case
    "text" -> pure TextItem
    t -> ImageItem <$> parseImageType t

data ImageType
  = JpgImage
  | PngImage
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Validity ImageType

parseImageType :: Text -> Maybe ImageType
parseImageType =
  \case
    "image/jpeg" -> Just JpgImage
    "image/png" -> Just PngImage
    _ -> Nothing

renderImageType :: ImageType -> Text
renderImageType =
  \case
    JpgImage -> "image/jpeg"
    PngImage -> "image/png"
