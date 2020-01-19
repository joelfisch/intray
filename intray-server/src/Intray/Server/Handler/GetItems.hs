{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetItems where

import Import

import Database.Persist

import Intray.API

import Intray.Server.Item
import Intray.Server.Types

import Intray.Server.Handler.Utils

serveGetItems :: AuthCookie -> IntrayHandler [ItemInfo TypedItem]
serveGetItems AuthCookie {..} = do
  itemsEnts <- runDb $ selectList [IntrayItemUserId ==. authCookieUserUUID] [Asc IntrayItemCreated]
  pure $ map (makeItemInfo . entityVal) itemsEnts
