{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.AccessKey.DeleteAccessKey
  ( serveDeleteAccessKey
  ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveDeleteAccessKey :: AuthResult AuthCookie -> AccessKeyUUID -> IntrayHandler NoContent
serveDeleteAccessKey (Authenticated AuthCookie {..}) uuid =
  withPermission authCookiePermissions PermitDeleteAccessKey $ do
    runDb $ deleteWhere [AccessKeyIdentifier ==. uuid]
    pure NoContent
serveDeleteAccessKey _ _ = throwAll err401
