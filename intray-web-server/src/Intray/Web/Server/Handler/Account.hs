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

import Intray.Web.Server.Handler.Pricing

getAccountR :: Handler Html
getAccountR =
  withLogin $ \t -> do
    mai <- runClientOrDisallow $ clientGetAccountInfo t
    mPricing <- runClientOrErr clientGetPricing
    accountInfoWidget <- accountInfoSegment mai mPricing
    token <- genToken
    withNavBar $(widgetFile "account")

accountInfoSegment :: Maybe AccountInfo -> Maybe Pricing -> Handler Widget
accountInfoSegment Nothing _ =
  pure
    [whamlet|
        <div .ui .negative .message>
            You are not authorised to view account info.|]
accountInfoSegment (Just ai@AccountInfo {..}) mp = do
  timestampWidget <- makeTimestampWidget accountInfoCreatedTimestamp
  pure $
    [whamlet|
        <div .ui .segment>
          <h1> Account

          <p> Username: #{usernameText accountInfoUsername}
          <p> Created: ^{timestampWidget}|] <>
    maybe mempty (pricingStripeForm ai) mp

pricingStripeForm :: AccountInfo -> Pricing -> Widget
pricingStripeForm AccountInfo {..} p =
  let Stripe.PlanId planText = pricingPlan p
      clientReferenceId = uuidText accountInfoUUID
      sf = $(widgetFile "stripe-form")
   in [whamlet|
        <div .ui .segment>
          <h2> Subscribe
          <p>
            <ul>
              <li>
                #{pricingShowAmountPerMonth p} per month, billed anually
              <li>
                Unlimited items
              <li>
                Full API access
              $maybe tp <- pricingTrialPeriod p
                <li>
                  #{show tp} day Trial period
          <p>
            ^{sf}
    |]

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
