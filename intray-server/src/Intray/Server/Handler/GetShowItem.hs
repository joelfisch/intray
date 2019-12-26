{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetShowItem where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils
import Intray.Server.Item

serveGetShowItem :: AuthResult AuthCookie -> IntrayHandler (Maybe (ItemInfo TypedItem))
serveGetShowItem (Authenticated AuthCookie {..}) =
  withPermission authCookiePermissions PermitShow $ do
    itemsEnt <-
      runDb $ selectFirst [IntrayItemUserId ==. authCookieUserUUID] [Asc IntrayItemCreated]
    pure $ makeItemInfo . entityVal <$> itemsEnt
serveGetShowItem _ = throwAll err401
