{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.PostAddItem
  ( servePostAddItem
  ) where

import Import

import Data.Time
import Data.UUID.Typed
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Intray.API

import Intray.Server.Handler.Stripe
import Intray.Server.Item
import Intray.Server.Types

import Intray.Server.Handler.Utils

servePostAddItem :: AuthCookie -> TypedItem -> IntrayHandler ItemUUID
servePostAddItem AuthCookie {..} typedItem = do
  ps <- getUserPaidStatus authCookieUserUUID
  case ps of
    HasNotPaid i ->
      if i >= 1
        then goAhead
        else throwAll err402
    HasPaid _ -> goAhead
    NoPaymentNecessary -> goAhead
  where
    goAhead = do
      now <- liftIO getCurrentTime
      uuid <- liftIO nextRandomUUID
      runDb $ insert_ $ makeIntrayItem authCookieUserUUID uuid now typedItem
      pure uuid
