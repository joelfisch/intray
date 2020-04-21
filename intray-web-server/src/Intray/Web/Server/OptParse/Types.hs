{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server.OptParse.Types where

import Data.Aeson
import Import
import qualified Intray.Server.OptParse.Types as API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
  CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags =
  ServeFlags
    { serveFlagAPIFlags :: !API.ServeFlags
    , serveFlagPort :: !(Maybe Int)
    , serveFlagPersistLogins :: !(Maybe Bool)
    , serveFlagTracking :: !(Maybe Text)
    , serveFlagVerification :: !(Maybe Text)
    }
  deriving (Show, Eq)

data Flags =
  Flags
    { flagAPIFlags :: !API.Flags
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
    { confAPIConfiguration :: !API.Configuration
    , confPort :: !(Maybe Int)
    , confPersistLogins :: !(Maybe Bool)
    , confTracking :: !(Maybe Text)
    , confVerification :: !(Maybe Text)
    }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON v =
    flip (withObject "Configuration") v $ \o -> do
      confAPIConfiguration <- parseJSON v
      confPort <- o .:? "web-port"
      confPersistLogins <- o .:? "persist-logins"
      confTracking <- o .:? "tracking"
      confVerification <- o .:? "verification"
      pure Configuration {..}

data Environment =
  Environment
    { envAPIEnvironment :: !API.Environment
    , envPort :: !(Maybe Int)
    , envPersistLogins :: !(Maybe Bool)
    , envTracking :: !(Maybe Text)
    , envVerification :: !(Maybe Text)
    }
  deriving (Show, Eq)

newtype Dispatch =
  DispatchServe ServeSettings
  deriving (Show)

data ServeSettings =
  ServeSettings
    { serveSetAPISettings :: !API.ServeSettings
    , serveSetPort :: !Int
    , serveSetPersistLogins :: !Bool
    , serveSetTracking :: !(Maybe Text)
    , serveSetVerification :: !(Maybe Text)
    }
  deriving (Show)

data Settings =
  Settings
  deriving (Show, Eq)
