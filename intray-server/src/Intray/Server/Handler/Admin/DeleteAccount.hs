{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.Admin.DeleteAccount
  ( serveAdminDeleteAccount
  ) where

import Import

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveAdminDeleteAccount :: AuthResult AuthCookie -> AccountUUID -> IntrayHandler NoContent
serveAdminDeleteAccount (Authenticated AuthCookie {..}) uuid =
  withPermission authCookiePermissions PermitAdminDeleteAccount $ do
    deleteAccountFully uuid
    pure NoContent
serveAdminDeleteAccount _ _ = throwAll err401
