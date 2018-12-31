{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Show
    ( showItem
    ) where

import Import

import qualified Data.Text as T
import Data.Time
import Text.Time.Pretty

import Intray.API

import Intray.Cli.OptParse
import Intray.Cli.Store
import Intray.Cli.Sync

showItem :: CliM ()
showItem = do
    mls <- readLastSeen
    mli <-
        case mls of
            Nothing -> syncAndReturn lastItemInStore
            Just li -> pure $ Just li
    case mli of
        Nothing -> liftIO $ putStrLn "Done."
        Just li -> do
            writeLastSeen li
            now <- liftIO getCurrentTime
            liftIO $ putStrLn $ prettyItem now li

prettyItem :: UTCTime -> LastItem -> String
prettyItem now li =
    let lastItemTimestamp =
            case li of
                LastItemUnsynced a -> addedCreated a
                LastItemSynced s -> syncedCreated s
        lastItemData =
            case li of
                LastItemUnsynced a -> addedValue a
                LastItemSynced s -> syncedValue s
        timeStr = prettyTimestamp now lastItemTimestamp
        timeAgoString = prettyTimeAuto now lastItemTimestamp
    in case typedItemCase lastItemData of
           Left err -> unlines ["Invalid item:", err]
           Right i ->
               case i of
                   CaseTextItem t ->
                       unlines
                           [ concat [timeStr, " (", timeAgoString, ")"]
                           , T.unpack t
                           ]

prettyTimestamp :: UTCTime -> UTCTime -> String
prettyTimestamp now d =
    let year = (\(y, _, _) -> y) . toGregorian . utctDay
    in (if year now == year d
            then formatTime defaultTimeLocale "%A %B %e at %H:%M"
            else formatTime defaultTimeLocale "%A %B %e %Y at %H:%M")
           d
