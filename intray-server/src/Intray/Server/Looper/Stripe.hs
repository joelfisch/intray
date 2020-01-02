{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Looper.Stripe
  ( runStripeLooper
  ) where

import Import

import Data.Aeson

import Web.Stripe as Stripe

import Intray.Server.Looper.Import
import Intray.Server.Stripe

runStripeLooper ::
     FromJSON (StripeReturn a) => StripeRequest a -> Looper (Either StripeError (StripeReturn a))
runStripeLooper request = do
  stripeSets <- asks looperEnvStripeSettings
  liftIO $ runStripeWith stripeSets request
