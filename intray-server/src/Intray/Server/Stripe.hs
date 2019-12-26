{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Stripe
  ( runStripe
  , runStripeOrError
  , runStripeWith
  , runStripeOrErrorWith
  ) where

import Import

import Control.Exception

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8

import Servant

import Web.Stripe as Stripe

import Intray.Server.Types

runStripeWith ::
     FromJSON (StripeReturn a)
  => MonetisationSettings
  -> StripeRequest a
  -> IntrayHandler (Either StripeError (StripeReturn a))
runStripeWith MonetisationSettings {..} request =
  liftIO $ stripe monetisationSetStripeConfig request

runStripe ::
     FromJSON (StripeReturn a)
  => StripeRequest a
  -> IntrayHandler (Maybe (Either StripeError (StripeReturn a)))
runStripe request = do
  mMonetisation <- asks envMonetisationSettings
  forM mMonetisation $ \ms -> runStripeWith ms request

runStripeOrError ::
     FromJSON (StripeReturn a) => StripeRequest a -> IntrayHandler (Maybe (StripeReturn a))
runStripeOrError request = do
  mMonetisation <- asks envMonetisationSettings
  forM mMonetisation $ \ms -> runStripeOrErrorWith ms request

runStripeOrErrorWith ::
     FromJSON (StripeReturn a)
  => MonetisationSettings
  -> StripeRequest a
  -> IntrayHandler (StripeReturn a)
runStripeOrErrorWith ms request = do
  errOrRes <- runStripeWith ms request
  case errOrRes of
    Left err -> throwError (err503 {errBody = LB8.pack $ displayException err})
    Right res -> pure res
