{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.AccessKey.GetAccessKeys
  ( serveGetAccessKeys
  ) where

import Import

import Database.Persist

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.AccessKey.GetAccessKey (makeAccessKeyInfo)
import Intray.Server.Handler.Utils

serveGetAccessKeys :: AuthCookie -> IntrayHandler [AccessKeyInfo]
serveGetAccessKeys AuthCookie {..} = do
  aks <- runDb $ selectList [AccessKeyUser ==. authCookieUserUUID] []
  pure $ map (makeAccessKeyInfo . entityVal) aks
