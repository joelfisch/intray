{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Server.Handler.Public.GetPricing
  ( serveGetPricing
  ) where

import Import

import Data.Cache as Cache

import Servant.Auth.Server.SetCookieOrphan ()

import Web.Stripe.Plan as Stripe

import Intray.API

import Intray.Server.Stripe
import Intray.Server.Types

serveGetPricing :: IntrayHandler (Maybe Pricing)
serveGetPricing = do
  mMonetisation <- asks envMonetisationSettings
  forM mMonetisation $ \ms@MonetisationSettings {..} -> do
    planCache <- asks envPlanCache
    mPlan <- liftIO $ Cache.lookup planCache monetisationSetPlan
    Stripe.Plan {..} <-
      case mPlan of
        Nothing -> do
          plan <- runStripeOrErrorWith ms $ getPlan monetisationSetPlan
          liftIO $ Cache.insert planCache monetisationSetPlan plan
          pure plan
        Just plan -> pure plan
    let pricingPlan = monetisationSetPlan
        pricingPrice = Stripe.Amount planAmount
        pricingCurrency = planCurrency
        pricingStripePublishableKey = monetisationSetStripePublishableKey
    pure Pricing {..}
