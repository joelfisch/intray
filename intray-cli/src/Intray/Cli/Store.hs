{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Store
  ( CS
  , ClientStore(..)
  , withClientStore
  , withClientStore'
  , readClientStore
    -- readClientStoreOrEmpty,
  , readClientStoreSize
    -- writeClientStore,
  , addItemToClientStore
  , storeSize
  , anyUnsynced
  , LastItem(..)
  , lastItemInClientStore
  , doneLastItem
  , writeLastSeen
  , readLastSeen
  , clearLastSeen
  , prettyReadyItem
  ) where

import Data.Aeson
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Mergeless
import qualified Data.Text as T
import Data.Time
import Import
import Intray.API
import Intray.Cli.JSON
import Intray.Cli.OptParse
import Intray.Cli.Path
import System.FileLock
import Text.Time.Pretty
import UnliftIO.Exception

type CS = ClientStore ClientId ItemUUID (AddedItem TypedItem)

withClientStore :: (CS -> CliM CS) -> CliM ()
withClientStore func = void $ withClientStore' (fmap ((,) ()) . func)

withClientStore' :: (CS -> CliM (a, CS)) -> CliM a
withClientStore' func = do
  p <- storePath
  bracket (liftIO $ lockFile (fromAbsFile p) Exclusive) (liftIO . unlockFile) $ \_ -> do
    before <- readClientStoreOrEmpty
    (r, after) <- func before
    writeClientStore after
    pure r

readClientStore :: CliM (Maybe CS)
readClientStore = do
  p <- storePath
  readJSON p $
    unlines
      [ "If you see this error, it means the way serialisation of the store has changed in a backward-incompatible way."
      , "As long as you have no unsynced items, you can just remove this file and re-sync: " <>
        fromAbsFile p
      , "If you do have unsynced items, you will want to make a backup of this file first."
      ]

readClientStoreOrEmpty :: CliM CS
readClientStoreOrEmpty = fromMaybe emptyClientStore <$> readClientStore

readClientStoreSize :: CliM Int
readClientStoreSize = storeSize <$> readClientStoreOrEmpty

writeClientStore :: CS -> CliM ()
writeClientStore s = do
  checkLastSeenAfter s
  storePath >>= (`writeJSON` s)

anyUnsynced :: CS -> Bool
anyUnsynced = not . M.null . clientStoreAdded

checkLastSeenAfter :: CS -> CliM ()
checkLastSeenAfter s = do
  mLs <- readLastSeen
  case mLs of
    Nothing -> pure () -- Nothing was last seen, cannot be out of date
    Just ls -> unless (lastSeenInClientStore ls s) clearLastSeen

lastSeenInClientStore :: LastItem -> CS -> Bool
lastSeenInClientStore li ClientStore {..} =
  case li of
    LastItemUnsynced ci a1 ->
      M.member ci clientStoreAdded ||
      elem a1 clientStoreSynced -- An unsynced item could have gotten synced.
    LastItemSynced uuid _ -> M.member uuid clientStoreSynced

data LastItem
  = LastItemSynced ItemUUID (AddedItem TypedItem)
  | LastItemUnsynced ClientId (AddedItem TypedItem)
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

lastItemInClientStore :: CS -> Maybe LastItem
lastItemInClientStore ClientStore {..} =
  let lasts =
        concat
          [ map (uncurry LastItemUnsynced) (M.toList clientStoreAdded)
          , map (uncurry LastItemSynced) (M.toList clientStoreSynced)
          ]
   in case lasts of
        [] -> Nothing
        (li:_) -> Just li

doneLastItem :: LastItem -> CS -> CS
doneLastItem li cs =
  case li of
    LastItemUnsynced ci _ -> deleteUnsyncedFromClientStore ci cs
    LastItemSynced u _ -> deleteSyncedFromClientStore u cs

prettyReadyItem :: UTCTime -> LastItem -> CliM String
prettyReadyItem now li =
  let idString =
        case li of
          LastItemUnsynced ci _ -> show (unClientId ci)
          LastItemSynced i _ -> uuidString i
      lastItemTimestamp =
        case li of
          LastItemUnsynced _ a -> addedItemCreated a
          LastItemSynced _ s -> addedItemCreated s
      lastItemData =
        case li of
          LastItemUnsynced _ a -> addedItemContents a
          LastItemSynced _ a -> addedItemContents a
      timeStr = prettyTimestamp now lastItemTimestamp
      timeAgoString = prettyTimeAuto now lastItemTimestamp
   in case typedItemCase lastItemData of
        Left err -> pure $ unlines ["Invalid item:", err]
        Right i -> do
          contents <-
            case i of
              CaseTextItem t -> pure $ T.unpack t
              CaseImageItem it bs -> do
                let ext =
                      case it of
                        JpgImage -> ".jpg"
                        PngImage -> ".png"
                cacheDir <- asks setCacheDir
                let fileName = idString ++ ext
                file <- resolveFile cacheDir fileName
                liftIO $ SB.writeFile (fromAbsFile file) bs
                pure $ "Image: " <> fromAbsFile file
          pure $ unlines [concat [timeStr, " (", timeAgoString, ")"], contents]

prettyTimestamp :: UTCTime -> UTCTime -> String
prettyTimestamp now d =
  let year = (\(y, _, _) -> y) . toGregorian . utctDay
   in (if year now == year d
         then formatTime defaultTimeLocale "%A %B %e at %H:%M"
         else formatTime defaultTimeLocale "%A %B %e %Y at %H:%M")
        d
