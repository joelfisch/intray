{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Stripe
  ( runStripeWith
  ) where

import Import

import Control.Exception

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8

import Servant

import Web.Stripe as Stripe

import Intray.Server.OptParse.Types
import Intray.Server.Types

runStripeWith ::
     FromJSON (StripeReturn a)
  => StripeSettings
  -> StripeRequest a
  -> IO (Either StripeError (StripeReturn a))
runStripeWith StripeSettings {..} = stripe stripeSetStripeConfig
