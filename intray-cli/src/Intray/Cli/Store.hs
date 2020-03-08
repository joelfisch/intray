{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Intray.Cli.Store
  ( ClientStore(..)
  , readClientStore
  , readClientStoreOrEmpty
  , writeClientStore
  , addItemToClientStore
  , LastItem(..)
  , lastItemInClientStore
  , doneLastItem
  , storeSize
  , writeLastSeen
  , readLastSeen
  , clearLastSeen
  , prettyItem
  ) where

import Import

import Data.Aeson
import qualified Data.Map as M
import Data.Mergeless
import qualified Data.Text as T
import Data.Time
import Text.Time.Pretty

import Intray.API

import Intray.Cli.JSON
import Intray.Cli.OptParse
import Intray.Cli.Path

{-# ANN module "HLint: ignore Use &&" #-}

{-# ANN module "HLint: ignore Use lambda-case" #-}

readClientStore :: CliM (Maybe (ClientStore ItemUUID TypedItem))
readClientStore = do
  p <- storePath
  readJSON p $
    unlines
      [ "If you see this error, it means the way serialisation of the store has changed in a backward-incompatible way."
      , "As long as you have no unsynced items, you can just remove this file and re-sync: " <>
        fromAbsFile p
      , "If you do have unsynced items, you will want to make a backup of this file first."
      ]

readClientStoreOrEmpty :: CliM (ClientStore ItemUUID TypedItem)
readClientStoreOrEmpty = fromMaybe emptyClientStore <$> readClientStore

writeClientStore :: ClientStore ItemUUID TypedItem -> CliM ()
writeClientStore s = do
  checkLastSeenAfter s
  storePath >>= (`writeJSON` s)

checkLastSeenAfter :: ClientStore ItemUUID TypedItem -> CliM ()
checkLastSeenAfter s = do
  mLs <- readLastSeen
  case mLs of
    Nothing -> pure () -- Nothing was last seen, cannot be out of date
    Just ls -> unless (lastSeenInClientStore ls s) clearLastSeen

lastSeenInClientStore :: LastItem -> ClientStore ItemUUID TypedItem -> Bool
lastSeenInClientStore li ClientStore {..} =
  case li of
    LastItemUnsynced ci Added {..} ->
      M.member ci clientStoreAdded ||
      any -- An unsynced item could have gotten synced.
        (\Synced {..} -> and [syncedCreated == addedCreated, syncedValue == addedValue])
        clientStoreSynced
    LastItemSynced uuid _ -> M.member uuid clientStoreSynced

data LastItem
  = LastItemSynced ItemUUID (Synced TypedItem)
  | LastItemUnsynced ClientId (Added TypedItem)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON LastItem

instance ToJSON LastItem

readLastSeen :: CliM (Maybe LastItem)
readLastSeen = do
  p <- lastSeenItemPath
  readJSON p $
    unlines
      [ "If you see this error, it means that the way serialisation of the last-seen item cache has changed in a backward-incompatible way."
      , "You can just remove this file and everything else will work as intended: " <> fromAbsFile p
      ]

writeLastSeen :: LastItem -> CliM ()
writeLastSeen i = do
  p <- lastSeenItemPath
  writeJSON p i

clearLastSeen :: CliM ()
clearLastSeen = do
  p <- lastSeenItemPath
  liftIO $ ignoringAbsence $ removeFile p

lastItemInClientStore :: ClientStore ItemUUID TypedItem -> Maybe LastItem
lastItemInClientStore ClientStore {..} =
  let lasts =
        concat
          [ map (uncurry LastItemUnsynced) (M.toList clientStoreAdded)
          , map (uncurry LastItemSynced) (M.toList clientStoreSynced)
          ]
   in case lasts of
        [] -> Nothing
        (li:_) -> Just li

doneLastItem :: LastItem -> ClientStore ItemUUID TypedItem -> ClientStore ItemUUID TypedItem
doneLastItem li cs =
  case li of
    LastItemUnsynced ci _ -> deleteUnsyncedFromClientStore ci cs
    LastItemSynced u _ -> deleteSyncedFromClientStore u cs

prettyItem :: UTCTime -> LastItem -> String
prettyItem now li =
  let lastItemTimestamp =
        case li of
          LastItemUnsynced _ a -> addedCreated a
          LastItemSynced _ s -> syncedCreated s
      lastItemData =
        case li of
          LastItemUnsynced _ a -> addedValue a
          LastItemSynced _ s -> syncedValue s
      timeStr = prettyTimestamp now lastItemTimestamp
      timeAgoString = prettyTimeAuto now lastItemTimestamp
   in case typedItemCase lastItemData of
        Left err -> unlines ["Invalid item:", err]
        Right i ->
          case i of
            CaseTextItem t -> unlines [concat [timeStr, " (", timeAgoString, ")"], T.unpack t]

prettyTimestamp :: UTCTime -> UTCTime -> String
prettyTimestamp now d =
  let year = (\(y, _, _) -> y) . toGregorian . utctDay
   in (if year now == year d
         then formatTime defaultTimeLocale "%A %B %e at %H:%M"
         else formatTime defaultTimeLocale "%A %B %e %Y at %H:%M")
        d
