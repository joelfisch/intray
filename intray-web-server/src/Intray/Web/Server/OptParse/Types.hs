{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.OptParse.Types where

import Data.Aeson
import Import
import qualified Intray.Server.OptParse.Types as API
import YamlParse.Applicative

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
  CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags =
  ServeFlags
    { serveFlagAPIFlags :: !API.ServeFlags
    , serveFlagPort :: !(Maybe Int)
    , serveFlagTracking :: !(Maybe Text)
    , serveFlagVerification :: !(Maybe Text)
    , serveFlagLoginCacheFile :: !(Maybe FilePath)
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
    , confTracking :: !(Maybe Text)
    , confVerification :: !(Maybe Text)
    , confLoginCacheFile :: !(Maybe FilePath)
    }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    (\apiConf (a, b, c, d) -> Configuration apiConf a b c d) <$> yamlSchema <*>
    objectParser
      "Configuration"
      ((,,,) <$> optionalField "web-port" "The port to serve web requests on" <*>
       optionalField "tracking" "The google analytics tracking code" <*>
       optionalField "verification" "The google search console verification code" <*>
       optionalField "login-cache-file" "The file to store the login cache database in")

data Environment =
  Environment
    { envAPIEnvironment :: !API.Environment
    , envPort :: !(Maybe Int)
    , envTracking :: !(Maybe Text)
    , envVerification :: !(Maybe Text)
    , envLoginCacheFile :: !(Maybe FilePath)
    }
  deriving (Show, Eq)

newtype Dispatch =
  DispatchServe ServeSettings
  deriving (Show)

data ServeSettings =
  ServeSettings
    { serveSetAPISettings :: !API.ServeSettings
    , serveSetPort :: !Int
    , serveSetTracking :: !(Maybe Text)
    , serveSetVerification :: !(Maybe Text)
    , serveSetLoginCacheFile :: !FilePath
    }
  deriving (Show)

data Settings =
  Settings
  deriving (Show, Eq)
