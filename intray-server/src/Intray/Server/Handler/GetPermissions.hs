{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetPermissions where

import Import

import Data.Set (Set)

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveGetPermissions :: AuthResult AuthCookie -> IntrayHandler (Set Permission)
serveGetPermissions (Authenticated AuthCookie {..}) =
  withPermission authCookiePermissions PermitGetPermissions $ pure authCookiePermissions
serveGetPermissions _ = throwAll err401
