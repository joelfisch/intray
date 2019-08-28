{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Server.Handler.Public.GetPricing
  ( serveGetPricing
  ) where

import Import

import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API

import Intray.Server.Types

serveGetPricing :: IntrayHandler (Maybe Pricing)
serveGetPricing = do
  mMonetisation <- asks envMonetisationSettings
  forM mMonetisation $ \MonetisationSettings {..} -> do
      let pricingPrice=monetisationSetPrice
          pricingStripePublishableKey=monetisationSetStripePublishableKey
      pure Pricing {..}
