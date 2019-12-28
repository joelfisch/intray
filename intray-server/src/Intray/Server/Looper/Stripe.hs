{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Looper.Stripe
  ( runStripeLooper
  ) where

import Import

import Control.Exception

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8

import Servant

import Web.Stripe as Stripe

import Intray.Server.Looper.Import
import Intray.Server.OptParse.Types
import Intray.Server.Stripe
import Intray.Server.Types

runStripeLooper ::
     FromJSON (StripeReturn a) => StripeRequest a -> Looper (Either StripeError (StripeReturn a))
runStripeLooper request = do
  stripeSets <- asks looperEnvStripeSettings
  liftIO $ runStripeWith stripeSets request
