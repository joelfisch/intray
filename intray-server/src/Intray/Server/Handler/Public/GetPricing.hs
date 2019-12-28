{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Public.GetPricing
  ( serveGetPricing
  ) where

import Import

import Data.Cache as Cache

import Servant.Auth.Server.SetCookieOrphan ()

import Web.Stripe.Plan as Stripe

import Intray.API

import Intray.Server.Handler.Stripe
import Intray.Server.OptParse.Types
import Intray.Server.Types

serveGetPricing :: IntrayHandler (Maybe Pricing)
serveGetPricing = do
  mStripe <- asks envStripeSettings
  forM mStripe $ \ms@StripeSettings {..} -> do
    planCache <- asks envPlanCache
    mPlan <- liftIO $ Cache.lookup planCache stripeSetPlan
    Stripe.Plan {..} <-
      case mPlan of
        Nothing -> do
          plan <- runStripeHandlerOrErrorWith ms $ getPlan stripeSetPlan
          liftIO $ Cache.insert planCache stripeSetPlan plan
          pure plan
        Just plan -> pure plan
    let pricingPlan = stripeSetPlan
        pricingTrialPeriod = planTrialPeriodDays
        pricingPrice = Stripe.Amount planAmount
        pricingCurrency = planCurrency
        pricingStripePublishableKey = stripeSetPublishableKey
    pure Pricing {..}
