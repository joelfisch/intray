{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.Admin.GetStats
  ( serveAdminGetStats
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

serveAdminGetStats :: AuthResult AuthCookie -> IntrayHandler AdminStats
serveAdminGetStats (Authenticated AuthCookie {..}) =
  withPermission authCookiePermissions PermitAdminGetStats $ do
    adminStatsNbUsers <- runDb $ count ([] :: [Filter User])
    adminStatsNbItems <- runDb $ count ([] :: [Filter IntrayItem])
    pure AdminStats {..}
serveAdminGetStats _ = throwAll err401
