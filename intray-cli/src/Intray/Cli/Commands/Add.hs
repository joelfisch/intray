{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Add
  ( addItem
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Import
import Intray.API
import Intray.Cli.OptParse
import Intray.Cli.Store
import Intray.Cli.Sync

addItem :: AddSettings -> CliM ()
addItem AddSettings {..} = do
  now <- liftIO getCurrentTime
  itemContents <-
    if addSetReadStdin
      then do
        cts <- liftIO T.getContents
        pure $ T.intercalate "\n" [addSetContents, cts]
      else pure addSetContents
  modifyClientStoreAndSync $
    addItemToClientStore
      AddedItem {addedItemContents = textTypedItem itemContents, addedItemCreated = now}
