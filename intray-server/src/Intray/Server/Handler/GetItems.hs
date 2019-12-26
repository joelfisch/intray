{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetItems where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Item
import Intray.Server.Types

import Intray.Server.Handler.Utils

serveGetItems :: AuthResult AuthCookie -> IntrayHandler [ItemInfo TypedItem]
serveGetItems (Authenticated AuthCookie {..}) =
  withPermission authCookiePermissions PermitGetItems $ do
    itemsEnts <-
      runDb $ selectList [IntrayItemUserId ==. authCookieUserUUID] [Asc IntrayItemCreated]
    pure $ map (makeItemInfo . entityVal) itemsEnts
serveGetItems _ = throwAll err401
