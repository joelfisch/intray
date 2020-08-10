{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
--   Based on https://github.com/dmjio/stripe/issues/89
--   but modified heavily.
module Web.Stripe.Conduit where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Network.HTTP.Simple as HS
import Safe (lastMay)
import qualified StripeAPI as Stripe
import qualified Web.Stripe as StripeOld
import Prelude

-- | Create a conduit request to `Stripe`'s API
stripeConduit ::
  ( MonadIO m,
    MonadThrow m
  ) =>
  StripeOld.StripeConfig ->
  Maybe Text ->
  ConduitT () Stripe.Event m ()
stripeConduit config startingAfter = do
  res <-
    liftIO
      $ Stripe.runWithConfiguration
        ( Stripe.defaultConfiguration
            { Stripe.configSecurityScheme =
                Stripe.basicAuthenticationSecurityScheme $
                  Stripe.BasicAuthenticationData
                    { Stripe.basicAuthenticationDataUsername = T.pack $ BS.unpack $ StripeOld.getStripeKey $ StripeOld.secretKey config,
                      Stripe.basicAuthenticationDataPassword = ""
                    }
            }
        )
      $ Stripe.getEvents
        Stripe.mkGetEventsParameters
          { Stripe.getEventsParametersQueryStartingAfter = startingAfter,
            Stripe.getEventsParametersQueryType = Just "checkout.session.completed"
          }
  case HS.getResponseBody res of
    Stripe.GetEventsResponse200 Stripe.GetEventsResponseBody200 {..} -> do
      CL.sourceList getEventsResponseBody200Data
      when getEventsResponseBody200HasMore $ do
        lastId <-
          case Stripe.eventId <$> lastMay getEventsResponseBody200Data of
            Just lastId -> pure lastId
            Nothing ->
              throwM
                StripeOld.StripeError
                  { StripeOld.errorType = StripeOld.APIError,
                    StripeOld.errorMsg = "Stripe returned an empty list",
                    StripeOld.errorCode = Nothing,
                    StripeOld.errorParam = Nothing,
                    StripeOld.errorHTTP = Nothing,
                    StripeOld.errorValue = Nothing
                  }
        stripeConduit config (Just lastId)
    Stripe.GetEventsResponseError err ->
      throwM
        StripeOld.StripeError
          { StripeOld.errorType = StripeOld.APIError,
            StripeOld.errorMsg = T.pack err,
            StripeOld.errorCode = Nothing,
            StripeOld.errorParam = Nothing,
            StripeOld.errorHTTP = Nothing,
            StripeOld.errorValue = Nothing
          }
    Stripe.GetEventsResponseDefault Stripe.Error {..} ->
      throwM
        StripeOld.StripeError
          { StripeOld.errorType = StripeOld.APIError,
            StripeOld.errorMsg = Maybe.fromMaybe "" $ Stripe.apiErrorsMessage errorError,
            StripeOld.errorCode = Nothing,
            StripeOld.errorParam = Stripe.apiErrorsParam errorError,
            StripeOld.errorHTTP = Nothing,
            StripeOld.errorValue = Nothing
          }
