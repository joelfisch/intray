{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.DeleteItem
  ( serveDeleteItem
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

serveDeleteItem :: AuthResult AuthCookie -> ItemUUID -> IntrayHandler NoContent
serveDeleteItem (Authenticated AuthCookie {..}) id_ =
  withPermission authCookiePermissions PermitDelete $ do
    runDb . deleteBy $ UniqueItemIdentifier id_
    pure NoContent
serveDeleteItem _ _ = throwAll err401
