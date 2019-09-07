module Intray.Web.Server.OptParse.Types where

import Import

import qualified Intray.Server.OptParse.Types as API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
  CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags =
  ServeFlags
    { serveFlagAPIFlags :: API.ServeFlags
    , serveFlagHost :: Maybe Text
    , serveFlagPort :: Maybe Int
    , serveFlagPersistLogins :: Maybe Bool
    , serveFlagTracking :: Maybe Text
    , serveFlagVerification :: Maybe Text
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
    { envAPIEnvironment :: API.Environment
    , envHost :: Maybe Text
    , envPort :: Maybe Int
    }
  deriving (Show, Eq)

newtype Dispatch =
  DispatchServe ServeSettings
  deriving (Show)

data ServeSettings =
  ServeSettings
    { serveSetAPISettings :: !API.ServeSettings
    , serveSetHost :: !(Maybe Text)
    , serveSetPort :: !Int
    , serveSetPersistLogins :: !Bool
    , serveSetTracking :: !(Maybe Text)
    , serveSetVerification :: !(Maybe Text)
    }
  deriving (Show)

data Settings =
  Settings
  deriving (Show, Eq)
