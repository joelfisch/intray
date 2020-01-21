{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Item
  ( makeIntrayItem
  , makeItemInfo
  , makeSynced
  ) where

import Data.Time

import Intray.API

makeIntrayItem :: AccountUUID -> ItemUUID -> UTCTime -> UTCTime -> TypedItem -> IntrayItem
makeIntrayItem u i at st TypedItem {..} =
  IntrayItem
    { intrayItemIdentifier = i
    , intrayItemType = itemType
    , intrayItemContents = itemData
    , intrayItemCreated = at
    , intrayItemSynced = st
    , intrayItemUserId = u
    }

makeItemInfo :: IntrayItem -> ItemInfo TypedItem
makeItemInfo IntrayItem {..} =
  ItemInfo
    { itemInfoIdentifier = intrayItemIdentifier
    , itemInfoContents = TypedItem {itemType = intrayItemType, itemData = intrayItemContents}
    , itemInfoTimestamp = intrayItemCreated
    }

makeSynced :: IntrayItem -> (ItemUUID, Synced TypedItem)
makeSynced IntrayItem {..} =
  ( intrayItemIdentifier
  , Synced
      { syncedValue = TypedItem {itemType = intrayItemType, itemData = intrayItemContents}
      , syncedCreated = intrayItemCreated
      , syncedSynced = intrayItemSynced
      })
