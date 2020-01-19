{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Public.GetPricing
  ( serveGetPricing
  ) where

import Import

import Data.Cache as Cache

import Web.Stripe.Plan as Stripe

import Intray.API

import Intray.Server.Handler.Stripe
import Intray.Server.OptParse.Types
import Intray.Server.Types

serveGetPricing :: IntrayHandler (Maybe Pricing)
serveGetPricing = do
  mMone <- asks envMonetisation
  forM mMone $ \MonetisationEnv {..} -> do
    planCache <- asks envPlanCache
    let StripeSettings {..} = monetisationEnvStripeSettings
    mPlan <- liftIO $ Cache.lookup planCache stripeSetPlan
    Stripe.Plan {..} <-
      case mPlan of
        Nothing -> do
          plan <- runStripeHandlerOrErrorWith monetisationEnvStripeSettings $ getPlan stripeSetPlan
          liftIO $ Cache.insert planCache stripeSetPlan plan
          pure plan
        Just plan -> pure plan
    let pricingPlan = stripeSetPlan
        pricingTrialPeriod = planTrialPeriodDays
        pricingPrice = Stripe.Amount planAmount
        pricingCurrency = planCurrency
        pricingStripePublishableKey = stripeSetPublishableKey
        pricingMaxItemsFree = monetisationEnvMaxItemsFree
    pure Pricing {..}
