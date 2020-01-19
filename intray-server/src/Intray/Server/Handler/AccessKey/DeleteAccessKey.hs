{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.AccessKey.DeleteAccessKey
  ( serveDeleteAccessKey
  ) where

import Import

import Database.Persist

import Servant

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveDeleteAccessKey :: AuthCookie -> AccessKeyUUID -> IntrayHandler NoContent
serveDeleteAccessKey AuthCookie {..} uuid = do
  runDb $ deleteWhere [AccessKeyIdentifier ==. uuid]
  pure NoContent
