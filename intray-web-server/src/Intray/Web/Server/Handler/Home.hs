{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Home
  ( getHomeR
  ) where

import Import

import Yesod

import Web.Stripe as Stripe
import Web.Stripe.Types as Stripe

import Intray.Client
import Intray.Web.Server.Foundation

getHomeR :: Handler Html
getHomeR = do
  mPricing <- runClientOrErr clientGetPricing
  withNavBar $(widgetFile "home")

showAmountForPricing :: Currency -> Amount -> String
showAmountForPricing cur (Amount i) =
  let (q, r) = quotRem i 100
      hundred =
        if r == 0
          then unwords [show q, show cur]
          else showAmount cur i
   in case cur of
        EUR -> hundred
        USD -> hundred
        CHF -> hundred
        _ -> showAmount cur i

quotPrice :: Amount -> Int -> Amount
quotPrice (Amount i) d = (Amount $ i `quot` d)
