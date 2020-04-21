{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.Server.Handler.GetAccountInfo
  ( serveGetAccountInfo
  , getAccountSubscribed
  ) where

import Data.Ord
import Data.Time
import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils
import Intray.Server.Types
import Servant
import Web.Stripe.Subscription as Stripe

serveGetAccountInfo :: AuthCookie -> IntrayHandler AccountInfo
serveGetAccountInfo AuthCookie {..} = do
  admins <- asks envAdmins
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ User {..}) -> do
      c <- runDb $ count ([IntrayItemUserId ==. authCookieUserUUID] :: [Filter IntrayItem])
      ups <- getUserPaidStatus authCookieUserUUID
      pure
        AccountInfo
          { accountInfoUUID = authCookieUserUUID
          , accountInfoUsername = userUsername
          , accountInfoCreatedTimestamp = userCreatedTimestamp
          , accountInfoLastLogin = userLastLogin
          , accountInfoAdmin = userUsername `elem` admins
          , accountInfoCount = c
          , accountInfoStatus = ups
          }

getAccountSubscribed :: AccountUUID -> IntrayHandler (Maybe UTCTime)
getAccountSubscribed aid = do
  mc <- runDb $ getBy $ UniqueCustomerUser aid
  case mc of
    Nothing -> pure Nothing -- No such customer on the stripe end, definitely hasn't paid then.
    Just (Entity _ Customer {..}) -> do
      mSubs <- runStripeHandlerOrError $ getSubscriptionsByCustomerId customerStripeCustomer
      case mSubs of
        Nothing -> pure Nothing -- Intray is being run for free
        Just subs -> do
          let relevantSubs =
                filter
                  ((\s -> s == Stripe.Active || s == Stripe.Trialing) . subscriptionStatus)
                  (Stripe.list subs)
          pure $
            case sortOn Down $ map subscriptionCurrentPeriodEnd relevantSubs of
              [] -> Nothing
              (end:_) -> Just end
