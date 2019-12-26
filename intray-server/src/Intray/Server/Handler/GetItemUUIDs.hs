{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetItemUUIDs
  ( serveGetItemUUIDs
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

serveGetItemUUIDs :: AuthResult AuthCookie -> IntrayHandler [ItemUUID]
serveGetItemUUIDs (Authenticated AuthCookie {..}) =
  withPermission authCookiePermissions PermitGetItemUUIDs $
  fmap (fmap $ intrayItemIdentifier . entityVal) $
  runDb $ selectList [IntrayItemUserId ==. authCookieUserUUID] [Asc IntrayItemCreated]
serveGetItemUUIDs _ = throwAll err401
