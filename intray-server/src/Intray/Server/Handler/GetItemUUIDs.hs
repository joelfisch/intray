{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetItemUUIDs
  ( serveGetItemUUIDs
  ) where

import Import

import Database.Persist

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveGetItemUUIDs :: AuthCookie -> IntrayHandler [ItemUUID]
serveGetItemUUIDs AuthCookie {..} =
  fmap (fmap $ intrayItemIdentifier . entityVal) $
  runDb $ selectList [IntrayItemUserId ==. authCookieUserUUID] [Asc IntrayItemCreated]
