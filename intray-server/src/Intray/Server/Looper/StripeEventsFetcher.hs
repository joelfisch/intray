{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Looper.StripeEventsFetcher where

import Web.Stripe.Event as Stripe

import Intray.Server.Looper.Import
import Intray.Server.Looper.Stripe

stripeEventsFetcherLooper :: Looper ()
stripeEventsFetcherLooper = do
  mEvents <- runStripeLooper getEvents -- TODO this will not work if there are many events
  forM_ mEvents $ \eventsL -> mapM handleEvent (Stripe.list eventsL)

handleEvent :: Stripe.Event -> Looper ()
handleEvent e@Stripe.Event {..} = liftIO $ pPrint e
