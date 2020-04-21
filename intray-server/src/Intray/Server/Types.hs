module Intray.Server.Types
  ( module Intray.Server.Types
  , MonetisationSettings(..)
  ) where

import Data.Cache
import Database.Persist.Sqlite
import Import
import Intray.API
import Intray.Server.OptParse.Types
import Servant
import Servant.Auth.Server
import Web.Stripe.Plan as Stripe

data IntrayServerEnv =
  IntrayServerEnv
    { envHost :: Text
    , envConnectionPool :: !ConnectionPool
    , envCookieSettings :: !CookieSettings
    , envJWTSettings :: !JWTSettings
    , envAdmins :: ![Username]
    , envFreeloaders :: ![Username]
    , envMonetisation :: !(Maybe MonetisationEnv)
    }

data MonetisationEnv =
  MonetisationEnv
    { monetisationEnvStripeSettings :: StripeSettings
    , monetisationEnvMaxItemsFree :: !Int
    , monetisationEnvPlanCache :: !(Cache Stripe.PlanId Stripe.Plan)
    }

type IntrayHandler = ReaderT IntrayServerEnv Handler
