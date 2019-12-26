{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.DeleteAccount
  ( serveDeleteAccount
  ) where

import Import

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveDeleteAccount :: AuthResult AuthCookie -> IntrayHandler NoContent
serveDeleteAccount (Authenticated AuthCookie {..}) =
  withPermission authCookiePermissions PermitDeleteAccount $ do
    deleteAccountFully authCookieUserUUID
    pure NoContent
serveDeleteAccount _ = throwAll err401
