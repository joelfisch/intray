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
  ) where

import Import

import Data.Aeson
import qualified Data.Map as M
import Data.Mergeless

import Intray.API

import Intray.Cli.JSON
import Intray.Cli.OptParse
import Intray.Cli.Path

{-# ANN module "HLint: ignore Use &&" #-}

{-# ANN module "HLint: ignore Use lambda-case" #-}

readClientStore :: CliM (Maybe (ClientStore ItemUUID TypedItem))
readClientStore = storePath >>= readJSON

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
  readJSON p

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
