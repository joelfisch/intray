{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.DeleteAccount
  ( serveDeleteAccount
  ) where

import Import

import Servant

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveDeleteAccount :: AuthCookie -> IntrayHandler NoContent
serveDeleteAccount AuthCookie {..} = do
  deleteAccountFully authCookieUserUUID
  pure NoContent
