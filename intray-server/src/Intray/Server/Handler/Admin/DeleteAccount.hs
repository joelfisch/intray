{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.Admin.DeleteAccount
  ( serveAdminDeleteAccount
  ) where

import Import

import Servant

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveAdminDeleteAccount :: AuthCookie -> AccountUUID -> IntrayHandler NoContent
serveAdminDeleteAccount AuthCookie {..} uuid = do
  deleteAccountFully uuid
  pure NoContent
