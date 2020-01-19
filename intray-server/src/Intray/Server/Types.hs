module Intray.Server.Types
  ( module Intray.Server.Types
  , MonetisationSettings(..)
  ) where

import Import

import Database.Persist.Sqlite

import Data.Cache

import Servant
import Servant.Auth.Server

import Web.Stripe.Plan as Stripe

import Intray.API

import Intray.Server.OptParse.Types

data IntrayServerEnv =
  IntrayServerEnv
    { envHost :: Text
    , envConnectionPool :: !ConnectionPool
    , envCookieSettings :: !CookieSettings
    , envJWTSettings :: !JWTSettings
    , envAdmins :: ![Username]
    , envMonetisation :: !(Maybe MonetisationEnv)
    }

data MonetisationEnv =
  MonetisationEnv
    { monetisationEnvStripeSettings :: !StripeSettings
    , monetisationEnvMaxItemsFree :: !Int
    , monetisationEnvPlanCache :: !(Cache Stripe.PlanId Stripe.Plan)
    }

type IntrayHandler = ReaderT IntrayServerEnv Handler
