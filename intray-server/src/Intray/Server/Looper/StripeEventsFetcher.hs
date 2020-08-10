{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Looper.StripeEventsFetcher where

import Conduit
import Data.Aeson
import Database.Persist
import Intray.Data
import Intray.Server.Looper.DB
import Intray.Server.Looper.Import
import Intray.Server.OptParse.Types
import qualified StripeAPI as Stripe
import Web.Stripe.Conduit
import qualified Web.Stripe.Types as StripeOld

stripeEventsFetcherLooper :: Looper ()
stripeEventsFetcherLooper = do
  stripeConfig <- asks (stripeSetStripeConfig . looperEnvStripeSettings)
  let fetchConduit =
        stripeConduit
          stripeConfig
          Nothing
  runConduit $ fetchConduit .| dealWithEventC

dealWithEventC :: ConduitT Stripe.Event Void Looper ()
dealWithEventC = do
  me <- await
  case me of
    Nothing -> pure ()
    Just e -> do
      lift $ dealWithEvent e
      dealWithEventC

dealWithEvent :: Stripe.Event -> Looper ()
dealWithEvent e = do
  mse <- looperDB $ getBy $ UniqueStripeEvent $ StripeOld.EventId $ Stripe.eventId e
  case mse of
    Just _ -> pure () -- No need to re-do this
    Nothing -> do
      se <- handleEvent e
      looperDB $ insert_ se

handleEvent :: Stripe.Event -> Looper StripeEvent
handleEvent Stripe.Event {..} =
  let err t = do
        logErr t
        pure $ StripeEvent {stripeEventEvent = StripeOld.EventId eventId, stripeEventError = Just t}
   in case eventType of
        "checkout.session.completed" ->
          case fromJSON (Object $ Stripe.notificationEventDataObject eventData) of
            Success Stripe.Checkout'session {..} ->
              case checkout'sessionClientReferenceId of
                Just crid ->
                  let maybeCid = case checkout'sessionCustomer of
                        Just (Stripe.Checkout'sessionCustomer'Customer c) -> Just $ StripeOld.CustomerId $ Stripe.customerId c
                        Just (Stripe.Checkout'sessionCustomer'Text cid) -> Just $ StripeOld.CustomerId cid
                        Nothing -> Nothing
                   in case maybeCid of
                        Just cid -> case parseUUID crid of
                          Just au -> completePayment (StripeOld.EventId eventId) au cid
                          Nothing -> err "Client reference id didn't look like an AccountUUID"
                        Nothing -> err "No customer id"
                Nothing -> err "No client reference id"
            _ -> err "Unknown event data"
        _ -> err "Unknown event"

completePayment :: StripeOld.EventId -> AccountUUID -> StripeOld.CustomerId -> Looper StripeEvent
completePayment eventId account cid = do
  void
    $ looperDB
    $ upsertBy
      (UniqueCustomerUser account)
      (Customer {customerUser = account, customerStripeCustomer = cid})
      [CustomerStripeCustomer =. cid]
  pure StripeEvent {stripeEventEvent = eventId, stripeEventError = Nothing}

logErr :: Text -> Looper ()
logErr = logErrorNS "stripe-events-fetcher"
