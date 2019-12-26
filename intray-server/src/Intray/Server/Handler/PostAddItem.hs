{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.PostAddItem
  ( servePostAddItem
  ) where

import Import

import Data.Time
import Data.UUID.Typed
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API

import Intray.Server.Item
import Intray.Server.Types

import Intray.Server.Handler.Utils

servePostAddItem :: AuthResult AuthCookie -> TypedItem -> IntrayHandler ItemUUID
servePostAddItem (Authenticated AuthCookie {..}) typedItem =
  withPermission authCookiePermissions PermitAdd $ do
    now <- liftIO getCurrentTime
    uuid <- liftIO nextRandomUUID
    runDb $ insert_ $ makeIntrayItem authCookieUserUUID uuid now typedItem
    pure uuid
servePostAddItem _ _ = throwAll err401
