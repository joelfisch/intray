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

import Servant

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveDeleteItem :: AuthCookie -> ItemUUID -> IntrayHandler NoContent
serveDeleteItem AuthCookie {..} id_ = do
  runDb . deleteBy $ UniqueItemIdentifier id_
  pure NoContent
