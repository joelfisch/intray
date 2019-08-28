{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Home
  ( getHomeR
  ) where

import Yesod

import Web.Stripe as Stripe
import Web.Stripe.Types as Stripe

import Intray.Client
import Intray.Web.Server.Foundation

getHomeR :: Handler Html
getHomeR = do
  mPricing <- runClientOrErr clientGetPricing
  withNavBar $(widgetFile "home")
