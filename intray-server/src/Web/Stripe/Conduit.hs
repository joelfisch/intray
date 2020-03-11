{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

   Based on https://github.com/dmjio/stripe/issues/89
   but modified heavily.

-}
module Web.Stripe.Conduit where

import Prelude

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Conduit
import Safe (lastMay)
import Web.Stripe ((-&-), stripe)

import qualified Data.Conduit.List as CL
import qualified Web.Stripe as Stripe
import qualified Web.Stripe.Customer as Stripe

-- | Create a conduit request to `Stripe`'s API
stripeConduit ::
     ( MonadIO m
     , FromJSON (Stripe.StripeReturn a)
     , Stripe.StripeReturn a ~ Stripe.StripeList b
     , Stripe.StripeHasParam a (Stripe.StartingAfter id)
     , MonadThrow m
     )
  => Stripe.StripeConfig
  -> Stripe.StripeRequest a
  -> (b -> id)
  -- ^ A mapping between the type and its ID field used in pagination
  -- TODO: the user should not have to set this themself
  -> ConduitT () b m ()
stripeConduit config request toId = do
  res <- liftIO $ stripe config request
  case res of
    Left e -> throwM e
    Right slist
        -- Yield all objs already present
     -> do
      let objs = Stripe.list slist
      CL.sourceList objs
        -- Paginate
      when (Stripe.hasMore slist) $ do
        lastId <-
          case toId <$> lastMay objs of
            Just lastId -> pure lastId
            Nothing ->
              throwM
                Stripe.StripeError
                  { Stripe.errorType = Stripe.APIError
                  , Stripe.errorMsg = "Stripe returned an empty list"
                  , Stripe.errorCode = Nothing
                  , Stripe.errorParam = Nothing
                  , Stripe.errorHTTP = Nothing
                  , Stripe.errorValue = Nothing
                  }
        stripeConduit config (request -&- Stripe.StartingAfter lastId) toId
