{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetPermissions where

import Import

import Data.Set (Set)

import Intray.API

import Intray.Server.Types

serveGetPermissions :: AuthCookie -> IntrayHandler (Set Permission)
serveGetPermissions AuthCookie {..} = pure authCookiePermissions
