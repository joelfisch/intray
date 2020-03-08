{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Item
  ( makeIntrayItem
  , makeItemInfo
  , makeAdded
  ) where

import Data.Time

import Intray.API

makeIntrayItem :: AccountUUID -> ItemUUID -> UTCTime -> TypedItem -> IntrayItem
makeIntrayItem u i at TypedItem {..} =
  IntrayItem
    { intrayItemIdentifier = i
    , intrayItemType = itemType
    , intrayItemContents = itemData
    , intrayItemCreated = at
    , intrayItemUserId = u
    }

makeItemInfo :: IntrayItem -> ItemInfo TypedItem
makeItemInfo IntrayItem {..} =
  ItemInfo
    { itemInfoIdentifier = intrayItemIdentifier
    , itemInfoContents = TypedItem {itemType = intrayItemType, itemData = intrayItemContents}
    , itemInfoCreated = intrayItemCreated
    }

makeAdded :: IntrayItem -> (ItemUUID, AddedItem TypedItem)
makeAdded IntrayItem {..} =
  ( intrayItemIdentifier
  , AddedItem
      { addedItemContents = TypedItem {itemType = intrayItemType, itemData = intrayItemContents}
      , addedItemCreated = intrayItemCreated
      })
