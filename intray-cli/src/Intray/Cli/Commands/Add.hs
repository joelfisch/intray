{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Intray.Cli.Commands.Add
  ( addItem
  ) where

import Import

import Data.Time

import Intray.API

import Intray.Cli.OptParse
import Intray.Cli.Store
import Intray.Cli.Sync

addItem :: Text -> CliM ()
addItem t = do
  now <- liftIO getCurrentTime
  modifyClientStoreAndSync $
    addItemToClientStore Added {addedValue = textTypedItem t, addedCreated = now}
