{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Intray.Cli.Store
    ( Store(..)
    , StoreItem(..)
    , readStore
    , readStoreOrEmpty
    , writeStore
    , addItemToStore
    , LastItem(..)
    , lastItemInStore
    , doneLastItem
    , storeSize
    , writeLastSeen
    , readLastSeen
    , clearLastSeen
    ) where

import Import

import Data.Aeson
import Data.Mergeless
import qualified Data.Set as S
import Data.Set (Set)

import Intray.API

import Intray.Cli.JSON
import Intray.Cli.OptParse
import Intray.Cli.Path

{-# ANN module "HLint: ignore Use &&" #-}

{-# ANN module "HLint: ignore Use lambda-case" #-}

readStore :: CliM (Maybe (Store ItemUUID TypedItem))
readStore = storePath >>= readJSON

readStoreOrEmpty :: CliM (Store ItemUUID TypedItem)
readStoreOrEmpty = fromMaybe emptyStore <$> readStore

writeStore :: Store ItemUUID TypedItem -> CliM ()
writeStore s = do
    checkLastSeenAfter s
    storePath >>= (`writeJSON` s)

checkLastSeenAfter :: Store ItemUUID TypedItem -> CliM ()
checkLastSeenAfter s = do
    mLs <- readLastSeen
    case mLs of
        Nothing -> pure () -- Nothing was last seen, cannot be out of date
        Just ls -> unless (lastSeenInStore ls s) clearLastSeen

lastSeenInStore :: LastItem -> Store ItemUUID TypedItem -> Bool
lastSeenInStore li Store {..} = any (`matches` li) storeItems

data LastItem
    = LastItemSynced (Synced ItemUUID TypedItem)
    | LastItemUnsynced (Added TypedItem)
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

lastItemInStore :: Store ItemUUID TypedItem -> Maybe LastItem
lastItemInStore (Store is) =
    let ls =
            flip mapSetMaybe is $ \ii ->
                case ii of
                    UnsyncedItem a -> Just $ LastItemUnsynced a
                    SyncedItem s -> Just $ LastItemSynced s
                    UndeletedItem _ -> Nothing
    in fst <$> S.minView ls

-- TODO maybe do this with an internal uuid?
doneLastItem :: LastItem -> Store ItemUUID TypedItem -> Store ItemUUID TypedItem
doneLastItem li (Store is) =
    Store $
    flip mapSetMaybe is $ \si ->
        case si of
            UnsyncedItem _ ->
                if matches si li
                    then Nothing
                    else Just si
            SyncedItem Synced {..} ->
                if matches si li
                    then Just (UndeletedItem syncedUuid)
                    else Just si
            UndeletedItem _ -> Just si

matches :: StoreItem ItemUUID TypedItem -> LastItem -> Bool
matches si li =
    case si of
        (UnsyncedItem uia) ->
            case li of
                LastItemUnsynced lia -> uia == lia
                LastItemSynced _ -> False
        (SyncedItem sis@Synced {..}) ->
            case li of
                LastItemSynced lis -> sis == lis
                LastItemUnsynced Added {..} ->
                    and
                        [ syncedCreated == addedCreated
                        , syncedValue == addedValue
                        ]
                    -- The item could have been synced after it has been shown.
                    -- In that case we will have a SyncedItem that should match a LastItemUnsynced
        (UndeletedItem _) -> False -- If it's deleted locally, we should report it's not in the store, even if we haven't synced yet.

mapSetMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapSetMaybe func = S.map fromJust . S.filter isJust . S.map func
