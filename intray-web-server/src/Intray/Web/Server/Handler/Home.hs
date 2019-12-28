{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Home
  ( getHomeR
  ) where

import Import

import Yesod

import Intray.Client
import Intray.Web.Server.Foundation
import Intray.Web.Server.Handler.Pricing

getHomeR :: Handler Html
getHomeR = do
  mPricing <- runClientOrErr clientGetPricing
  withNavBar $(widgetFile "home")
