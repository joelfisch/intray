{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetIntraySize
  ( serveGetIntraySize
  ) where

import Import

import Database.Persist

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveGetIntraySize :: AuthCookie -> IntrayHandler Int
serveGetIntraySize AuthCookie {..} = runDb $ count [IntrayItemUserId ==. authCookieUserUUID]
