{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetAccountInfo
  ( serveGetAccountInfo
  , getAccountSubscribed
  ) where

import Import

import Data.Ord
import Data.Time

import Database.Persist
import Web.Stripe.Subscription as Stripe

import Servant

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils

serveGetAccountInfo :: AuthCookie -> IntrayHandler AccountInfo
serveGetAccountInfo AuthCookie {..} = do
  admins <- asks envAdmins
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ User {..}) -> do
      c <- runDb $ count ([IntrayItemUserId ==. authCookieUserUUID] :: [Filter IntrayItem])
      ups <- getUserPaidStatus authCookieUserUUID
      let subbed =
            case ups of
              HasNotPaid _ -> Nothing
              HasPaid u -> Just u
              NoPaymentNecessary -> Nothing
      pure
        AccountInfo
          { accountInfoUUID = authCookieUserUUID
          , accountInfoUsername = userUsername
          , accountInfoCreatedTimestamp = userCreatedTimestamp
          , accountInfoLastLogin = userLastLogin
          , accountInfoAdmin = userUsername `elem` admins
          , accountInfoCount = c
          , accountInfoSubscribed = subbed
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
