{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.OptParse.Types where

import Control.Monad.Logger
import Data.Aeson
import Database.Persist.Sqlite
import Import
import Intray.API
import Looper
import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
  CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags =
  ServeFlags
    { serveFlagHost :: !(Maybe String)
    , serveFlagPort :: !(Maybe Int)
    , serveFlagDb :: !(Maybe Text)
    , serveFlagAdmins :: ![String]
    , serveFlagFreeloaders :: ![String]
    , serveFlagLogLevel :: Maybe LogLevel
    , serveFlagStripePlan :: !(Maybe String)
    , serveFlagStripeSecretKey :: !(Maybe String)
    , serveFlagStripePublishableKey :: !(Maybe String)
    , serveFlagLooperStripeEventsFetcher :: LooperFlags
    , serveFlagLooperStripeEventsRetrier :: LooperFlags
    , serveFlagMaxItemsFree :: !(Maybe Int)
    }
  deriving (Show, Eq)

data Flags =
  Flags
    { flagConfigFile :: !(Maybe FilePath)
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
    { confHost :: !(Maybe String)
    , confPort :: !(Maybe Int)
    , confDb :: !(Maybe Text)
    , confAdmins :: !(Maybe [String])
    , confFreeloaders :: !(Maybe [String])
    , confLogLevel :: !(Maybe LogLevel)
    , confMonetisationConfig :: !(Maybe MonetisationConfiguration)
    }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON =
    withObject "Configuration" $ \o ->
      Configuration <$> o .:? "api-host" <*> o .:? "api-port" <*> o .:? "database" <*>
      o .:? "admins" <*>
      o .:? "freeloaders" <*>
      (do ms <- o .:? "log-level"
          forM ms $ \s ->
            case readMaybe s of
              Nothing -> Import.fail $ "Unknown log level: " <> s
              Just ll -> pure ll) <*>
      o .:? "monetisation"

data MonetisationConfiguration =
  MonetisationConfiguration
    { monetisationConfStripePlan :: !(Maybe String)
    , monetisationConfStripeSecretKey :: !(Maybe String)
    , monetisationConfStripePublishableKey :: !(Maybe String)
    , monetisationConfStripeEventsFetcher :: !(Maybe LooperConfiguration)
    , monetisationConfStripeEventsRetrier :: !(Maybe LooperConfiguration)
    , monetisationConfMaxItemsFree :: !(Maybe Int)
    }
  deriving (Show, Eq)

instance FromJSON MonetisationConfiguration where
  parseJSON =
    withObject "MonetisationConfiguration" $ \o ->
      MonetisationConfiguration <$> o .:? "stripe-plan" <*> o .:? "stripe-secret-key" <*>
      o .:? "stripe-publishable-key" <*>
      o .:? "events-fetcher" <*>
      o .:? "events-retrier" <*>
      o .:? "max-items-free"

data Environment =
  Environment
    { envConfigFile :: !(Maybe FilePath)
    , envHost :: !(Maybe String)
    , envPort :: !(Maybe Int)
    , envDb :: !(Maybe Text)
    , envLogLevel :: !(Maybe LogLevel)
    , envStripePlan :: !(Maybe String)
    , envStripeSecretKey :: !(Maybe String)
    , envStripePublishableKey :: !(Maybe String)
    , envLooperStripeEventsFetcher :: LooperEnvironment
    , envLooperStripeEventsRetrier :: LooperEnvironment
    , envMaxItemsFree :: !(Maybe Int)
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
    { serveSetHost :: !Text
    , serveSetPort :: !Int
    , serveSetLogLevel :: !LogLevel
    , serveSetConnectionInfo :: !SqliteConnectionInfo
    , serveSetAdmins :: ![Username]
    , serveSetFreeloaders :: ![Username]
    , serveSetMonetisationSettings :: !(Maybe MonetisationSettings)
    }
  deriving (Show)

data MonetisationSettings =
  MonetisationSettings
    { monetisationSetStripeSettings :: !StripeSettings
    , monetisationSetStripeEventsFetcher :: LooperSettings
    , monetisationSetStripeEventsRetrier :: LooperSettings
    , monetisationSetMaxItemsFree :: !Int
    }
  deriving (Show)

data StripeSettings =
  StripeSettings
    { stripeSetPlan :: !Stripe.PlanId
    , stripeSetStripeConfig :: StripeConfig
    , stripeSetPublishableKey :: Text
    }
  deriving (Show)
