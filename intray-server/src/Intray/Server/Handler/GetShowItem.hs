{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetShowItem where

import Import

import Database.Persist

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils
import Intray.Server.Item

serveGetShowItem :: AuthCookie -> IntrayHandler (Maybe (ItemInfo TypedItem))
serveGetShowItem AuthCookie {..} = do
  itemsEnt <- runDb $ selectFirst [IntrayItemUserId ==. authCookieUserUUID] [Asc IntrayItemCreated]
  pure $ makeItemInfo . entityVal <$> itemsEnt
