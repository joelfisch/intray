{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Types
  ( ProtectAPI
  , AuthCookie(..)
  , Permission(..)
  , userPermissions
  , adminPermissions
  , Registration(..)
  , LoginForm(..)
  , GetDocsResponse(..)
  , HashedPassword
  , passwordHash
  , validatePassword
  , ItemUUID
  , AccountUUID
  , AccessKeyUUID
  , Username
  , parseUsername
  , parseUsernameWithError
  , usernameText
  , Pricing(..)
  , module Data.UUID.Typed
  ) where

import Import

import Data.Aeson as JSON
import Data.Hashable
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as UUID
import Data.UUID.Typed
import System.IO.Unsafe

import Text.Blaze as HTML
import Text.Blaze.Html as HTML

import Web.Cookie

import Servant.API
import Servant.Auth
import Servant.Auth.Docs ()
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Docs
import Servant.HTML.Blaze

import qualified Web.Stripe.Plan as Stripe

import Intray.Data

type ProtectAPI = Auth '[ JWT] AuthCookie

data AuthCookie =
  AuthCookie
    { authCookieUserUUID :: AccountUUID
    , authCookiePermissions :: Set Permission
    }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

instance FromJWT Permission

instance ToJWT Permission

instance ToCapture (Capture "uuid" ItemUUID) where
  toCapture _ = DocCapture "uuid" "The UUID of the item"

instance ToCapture (Capture "uuid" AccountUUID) where
  toCapture _ = DocCapture "uuid" "The UUID of the account"

instance ToCapture (Capture "uuid" AccessKeyUUID) where
  toCapture _ = DocCapture "uuid" "The UUID of the access key"

instance ToSample UTCTime where
  toSamples Proxy = singleSample $ UTCTime (fromGregorian 2018 2 10) 42

instance ToSample Text where
  toSamples Proxy = singleSample "Example Text"

instance ToSample (UUID a) where
  toSamples Proxy = singleSample (UUID $ UUID.fromWords 0 0 0 0)

instance ToSample Int where
  toSamples Proxy = singleSample 42

data Registration =
  Registration
    { registrationUsername :: Username
    , registrationPassword :: Text
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity Registration

instance ToJSON Registration where
  toJSON Registration {..} =
    object ["name" .= registrationUsername, "password" .= registrationPassword]

instance FromJSON Registration where
  parseJSON =
    withObject "Registration Text" $ \o -> Registration <$> o .: "name" <*> o .: "password"

instance ToSample Registration

data LoginForm =
  LoginForm
    { loginFormUsername :: Username
    , loginFormPassword :: Text
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance FromJSON LoginForm where
  parseJSON = withObject "LoginForm" $ \o -> LoginForm <$> o .: "username" <*> o .: "password"

instance ToJSON LoginForm where
  toJSON LoginForm {..} = object ["username" .= loginFormUsername, "password" .= loginFormPassword]

instance ToSample LoginForm

instance ToSample Username

instance ToSample SetCookie where
  toSamples Proxy = singleSample def

newtype GetDocsResponse =
  GetDocsResponse
    { unGetDocsResponse :: HTML.Html
    }
  deriving (Generic)

instance MimeUnrender HTML GetDocsResponse where
  mimeUnrender Proxy bs = Right $ GetDocsResponse $ HTML.unsafeLazyByteString bs

instance ToSample GetDocsResponse where
  toSamples Proxy = singleSample $ GetDocsResponse "Documentation (In HTML)."

instance ToMarkup GetDocsResponse where
  toMarkup (GetDocsResponse html) = toMarkup html

instance ToSample Permission

instance (Ord a, ToSample a) => ToSample (Set a) where
  toSamples Proxy = second S.fromList <$> toSamples Proxy

instance ToSample AccessKeySecret where
  toSamples Proxy = singleSample $ unsafePerformIO generateRandomAccessKeySecret

data Pricing =
  Pricing
    { pricingPlan :: !Stripe.PlanId
    , pricingTrialPeriod :: !(Maybe Int)
    , pricingPrice :: !Stripe.Amount
    , pricingCurrency :: !Stripe.Currency
    , pricingStripePublishableKey :: !Text
    }
  deriving (Show, Eq, Generic)

instance Validity Pricing

instance FromJSON Pricing where
  parseJSON =
    withObject "Pricing" $ \o ->
      Pricing <$> o .: "plan" <*> o .:? "trial-period" <*> o .: "price" <*> o .: "currency" <*>
      o .: "publishable-key"

instance ToJSON Pricing where
  toJSON Pricing {..} =
    object
      [ "plan" .= pricingPlan
      , "trial-period" .= pricingTrialPeriod
      , "price" .= pricingPrice
      , "currency" .= pricingCurrency
      , "publishable-key" .= pricingStripePublishableKey
      ]

instance ToSample Pricing where
  toSamples Proxy =
    singleSample
      Pricing
        { pricingPrice = Stripe.Amount 100
        , pricingTrialPeriod = Just 30
        , pricingCurrency = Stripe.CHF
        , pricingPlan = Stripe.PlanId "plan_FiN2Zsdv0DP0kh"
        , pricingStripePublishableKey = "pk_test_zV5qVP1IQTjE9QYulRZpfD8C00cqGOnQ91"
        }

instance Validity Stripe.Currency where
  validate = trivialValidation

instance ToJSON Stripe.Currency where
  toJSON c = toJSON $ T.toLower $ T.pack $ show c

deriving instance Validity Stripe.PlanId

deriving instance Hashable Stripe.PlanId

deriving instance ToJSON Stripe.PlanId

deriving instance FromJSON Stripe.PlanId

instance Validity Stripe.Amount where
  validate (Stripe.Amount a) = delve "getAmount" a

instance ToJSON Stripe.Amount where
  toJSON (Stripe.Amount a) = toJSON a

instance FromJSON Stripe.Amount where
  parseJSON v = Stripe.Amount <$> parseJSON v
