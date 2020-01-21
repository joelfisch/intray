{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Stripe
  ( runStripeWith
  ) where

import Import

import Data.Aeson

import Web.Stripe as Stripe (StripeError, StripeRequest, StripeReturn, stripe)

import Intray.Server.OptParse.Types

runStripeWith ::
     FromJSON (StripeReturn a)
  => StripeSettings
  -> StripeRequest a
  -> IO (Either StripeError (StripeReturn a))
runStripeWith StripeSettings {..} = stripe stripeSetStripeConfig
