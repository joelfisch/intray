{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Item
  ( makeIntrayItem
  , makeItemInfo
  , makeAdded
  ) where

import qualified Data.Text.Encoding as TE
import Data.Time

import Intray.API

makeIntrayItem :: AccountUUID -> ItemUUID -> UTCTime -> TypedItem -> IntrayItem
makeIntrayItem u i at TypedItem {..} =
  IntrayItem
    { intrayItemIdentifier = i
    , intrayItemType = itemType
    , intrayItemContents =
        case itemType of
          TextItem -> TE.encodeUtf8 itemData
    , intrayItemCreated = at
    , intrayItemUserId = u
    }

makeItemInfo :: IntrayItem -> ItemInfo TypedItem
makeItemInfo IntrayItem {..} =
  ItemInfo
    { itemInfoIdentifier = intrayItemIdentifier
    , itemInfoContents =
        TypedItem
          { itemType = intrayItemType
          , itemData =
              case intrayItemType of
                TextItem -> TE.decodeUtf8 intrayItemContents
          }
    , itemInfoCreated = intrayItemCreated
    }

makeAdded :: IntrayItem -> (ItemUUID, AddedItem TypedItem)
makeAdded IntrayItem {..} =
  ( intrayItemIdentifier
  , AddedItem
      { addedItemContents =
          TypedItem
            { itemType = intrayItemType
            , itemData =
                case intrayItemType of
                  TextItem -> TE.decodeUtf8 intrayItemContents
            }
      , addedItemCreated = intrayItemCreated
      })
