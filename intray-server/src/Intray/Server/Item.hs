{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Item
    ( makeIntrayItem
    , makeItemInfo
    , makeSynced
    ) where

import Data.Time

import Intray.API
import Intray.Data

makeIntrayItem :: AccountUUID -> ItemUUID -> UTCTime -> TypedItem -> IntrayItem
makeIntrayItem u i ts TypedItem {..} =
    IntrayItem
    { intrayItemIdentifier = i
    , intrayItemType = itemType
    , intrayItemContents = itemData
    , intrayItemCreated = ts
    , intrayItemSynced = ts
    , intrayItemUserId = u
    }

makeItemInfo :: IntrayItem -> ItemInfo TypedItem
makeItemInfo IntrayItem {..} =
    ItemInfo
    { itemInfoIdentifier = intrayItemIdentifier
    , itemInfoContents =
          TypedItem {itemType = intrayItemType, itemData = intrayItemContents}
    , itemInfoTimestamp = intrayItemCreated
    }

makeSynced :: IntrayItem -> Synced ItemUUID TypedItem
makeSynced IntrayItem {..} =
    Synced
    { syncedUuid = intrayItemIdentifier
    , syncedValue =
          TypedItem {itemType = intrayItemType, itemData = intrayItemContents}
    , syncedCreated = intrayItemCreated
    , syncedSynced = intrayItemSynced
    }
