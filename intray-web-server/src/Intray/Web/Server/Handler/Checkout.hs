{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server.Handler.Checkout
  ( getCheckoutSuccessR
  , getCheckoutCanceledR
  ) where

import Import

import Yesod
import Yesod.Auth

import Intray.Client

import Intray.Web.Server.Foundation
import Intray.Web.Server.Time

getCheckoutSuccessR :: Handler Html
getCheckoutSuccessR = redirect AccountR
getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = redirect AccountR
