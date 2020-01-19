{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetItem
  ( serveGetItem
  ) where

import Import

import Database.Persist

import Servant

import Intray.API

import Intray.Server.Item
import Intray.Server.Types

import Intray.Server.Handler.Utils

serveGetItem :: AuthCookie -> ItemUUID -> IntrayHandler (ItemInfo TypedItem)
serveGetItem AuthCookie {..} id_ = do
  mitem <- runDb $ getBy $ UniqueItemIdentifier id_
  case mitem of
    Nothing -> throwError err404 {errBody = "Item not found."}
    Just item -> pure $ makeItemInfo $ entityVal item
