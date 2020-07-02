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
  mItemContents <-
    case (addSetReadStdin, addSetContents) of
      (False, []) -> pure Nothing
      (True, []) -> Just <$> liftIO T.getContents
      (False, cts) -> pure $ Just $ T.unwords cts
      (True, cts) ->
        Just <$> do
          cts' <- liftIO T.getContents
          pure $ T.intercalate "\n" [T.unwords cts, cts']
  forM_ mItemContents $ \contents ->
    modifyClientStoreAndSync $
    addItemToClientStore
      AddedItem {addedItemContents = textTypedItem contents, addedItemCreated = now}
