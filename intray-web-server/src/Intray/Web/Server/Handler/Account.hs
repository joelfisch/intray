{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server.Handler.Account
  ( getAccountR
  , postAccountDeleteR
  ) where

import Import

import Yesod
import Yesod.Auth

import Text.Julius

import Web.Stripe.Plan as Stripe

import Intray.Client

import Intray.Web.Server.Foundation
import Intray.Web.Server.Time

getAccountR :: Handler Html
getAccountR =
  withLogin $ \t -> do
    mai <- runClientOrDisallow $ clientGetAccountInfo t
    accountInfoWidget <- accountInfoSegment mai
    token <- genToken
    mPricing <- runClientOrErr clientGetPricing
    mSubscribeForm <-
      forM mPricing $ \Pricing {..} -> do
        let Stripe.PlanId pricingPlanText = pricingPlan
        pure $(widgetFile "stripe-form")
    withNavBar $(widgetFile "account")

accountInfoSegment :: Maybe AccountInfo -> Handler Widget
accountInfoSegment Nothing =
  pure
    [whamlet|
        <div .ui .negative .message>
            You are not authorised to view account info.|]
accountInfoSegment (Just AccountInfo {..}) = do
  timestampWidget <- makeTimestampWidget accountInfoCreatedTimestamp
  pure
    [whamlet|
        <div .ui .segment>
          <h1> Account

          <p> Username: #{usernameText accountInfoUsername}
          <p> Created: ^{timestampWidget}|]

adminSegment :: Maybe AccountInfo -> Widget
adminSegment Nothing = mempty
adminSegment (Just AccountInfo {..})
  | accountInfoAdmin =
    [whamlet|
            <div .ui .segment>
                <p>
                  This account is an administrator.
                <p>
                  <a .ui .positive .button href=@{AdminR}>
                    The Admin Panel|]
  | otherwise = mempty

postAccountDeleteR :: Handler Html
postAccountDeleteR =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteAccount t
    clearCreds False
    redirect HomeR
