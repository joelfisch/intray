{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetIntraySize
  ( serveGetIntraySize
  ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveGetIntraySize :: AuthResult AuthCookie -> IntrayHandler Int
serveGetIntraySize (Authenticated AuthCookie {..}) =
  withPermission authCookiePermissions PermitSize $
  runDb $ count [IntrayItemUserId ==. authCookieUserUUID]
serveGetIntraySize _ = throwAll err401
