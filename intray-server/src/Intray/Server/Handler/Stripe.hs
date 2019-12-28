{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Handler.Stripe
  ( runStripeHandler
  , runStripeHandlerOrError
  , runStripeHandlerOrErrorWith
  ) where

import Import

import Control.Exception

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8

import Servant

import Web.Stripe as Stripe

import Intray.Server.OptParse.Types
import Intray.Server.Stripe
import Intray.Server.Types

runStripeHandler ::
     FromJSON (StripeReturn a)
  => StripeRequest a
  -> IntrayHandler (Maybe (Either StripeError (StripeReturn a)))
runStripeHandler request = do
  mStripeSets <- asks envStripeSettings
  forM mStripeSets $ \ms -> liftIO $ runStripeWith ms request

runStripeHandlerOrError ::
     FromJSON (StripeReturn a) => StripeRequest a -> IntrayHandler (Maybe (StripeReturn a))
runStripeHandlerOrError request = do
  mStripeSets <- asks envStripeSettings
  forM mStripeSets $ \ms -> runStripeHandlerOrErrorWith ms request

runStripeHandlerOrErrorWith ::
     FromJSON (StripeReturn a)
  => StripeSettings
  -> StripeRequest a
  -> IntrayHandler (StripeReturn a)
runStripeHandlerOrErrorWith ms request = do
  errOrRes <- liftIO $ runStripeWith ms request
  case errOrRes of
    Left err -> throwError (err503 {errBody = LB8.pack $ displayException err})
    Right res -> pure res
