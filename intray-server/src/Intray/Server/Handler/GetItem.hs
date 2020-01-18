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

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API

import Intray.Server.Item
import Intray.Server.Types

import Intray.Server.Handler.Utils

serveGetItem :: AuthResult AuthCookie -> ItemUUID -> IntrayHandler (ItemInfo TypedItem)
serveGetItem (Authenticated AuthCookie {..}) id_ =
  withPermission authCookiePermissions PermitGetItem $ do
    mitem <- runDb $ getBy $ UniqueItemIdentifier id_
    case mitem of
      Nothing -> throwError err404 {errBody = "Item not found."}
      Just item -> pure $ makeItemInfo $ entityVal item
serveGetItem _ _ = throwAll err401
