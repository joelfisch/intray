module Intray.Server.OptParse.Types where

import Import

import Control.Monad.Logger
import Database.Persist.Sqlite

import Looper

import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe

import Intray.API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
  CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags =
  ServeFlags
    { serveFlagPort :: !(Maybe Int)
    , serveFlagDb :: !(Maybe Text)
    , serveFlagAdmins :: ![String]
    , serveFlagLogLevel :: Maybe LogLevel
    , serveFlagStripePlan :: !(Maybe String)
    , serveFlagStripeSecretKey :: !(Maybe String)
    , serveFlagStripePublishableKey :: !(Maybe String)
    , serveFlagLooperStripeEventsFetcher :: LooperFlags
    , serveFlagLooperStripeEventsRetrier :: LooperFlags
    }
  deriving (Show, Eq)

data Flags =
  Flags
  deriving (Show, Eq)

data Configuration =
  Configuration
  deriving (Show, Eq)

data Environment =
  Environment
    { envPort :: !(Maybe Int)
    , envLogLevel :: !(Maybe LogLevel)
    , envStripePlan :: !(Maybe String)
    , envStripeSecretKey :: !(Maybe String)
    , envStripePublishableKey :: !(Maybe String)
    , envLooperStripeEventsFetcher :: LooperEnvironment
    , envLooperStripeEventsRetrier :: LooperEnvironment
    }
  deriving (Show, Eq)

newtype Dispatch =
  DispatchServe ServeSettings
  deriving (Show)

data Settings =
  Settings
  deriving (Show, Eq)

data ServeSettings =
  ServeSettings
    { serveSetPort :: !Int
    , serveSetLogLevel :: !LogLevel
    , serveSetConnectionInfo :: !SqliteConnectionInfo
    , serveSetAdmins :: ![Username]
    , serveSetMonetisationSettings :: !(Maybe MonetisationSettings)
    }
  deriving (Show)

data MonetisationSettings =
  MonetisationSettings
    { monetisationSetStripeSettings :: !StripeSettings
    , monetisationSetStripeEventsFetcher :: LooperSettings
    , monetisationSetStripeEventsRetrier :: LooperSettings
    }
  deriving (Show)

data StripeSettings =
  StripeSettings
    { stripeSetPlan :: !Stripe.PlanId
    , stripeSetStripeConfig :: !StripeConfig
    , stripeSetPublishableKey :: !Text
    }
  deriving (Show)
