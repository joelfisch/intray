{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.OptParse.Types where

import Import

import Data.Text (Text)
import Data.Yaml as Yaml

import Servant.Client

import Intray.Data

data Arguments =
  Arguments Command Flags
  deriving (Show, Eq, Generic)

data Instructions =
  Instructions Dispatch Settings
  deriving (Show, Eq, Generic)

data Command
  = CommandRegister RegisterArgs
  | CommandLogin LoginArgs
  | CommandPostPostAddItem [String]
  | CommandShowItem
  | CommandDoneItem
  | CommandSize
  | CommandReview
  | CommandLogout
  | CommandSync
  deriving (Show, Eq, Generic)

data RegisterArgs =
  RegisterArgs
    { registerArgUsername :: Maybe String
    , registerArgPassword :: Maybe String
    }
  deriving (Show, Eq, Generic)

data LoginArgs =
  LoginArgs
    { loginArgUsername :: Maybe String
    , loginArgPassword :: Maybe String
    }
  deriving (Show, Eq, Generic)

data Flags =
  Flags
    { flagConfigFile :: Maybe FilePath
    , flagUrl :: Maybe String
    , flagCacheDir :: Maybe FilePath
    , flagDataDir :: Maybe FilePath
    , flagSyncStrategy :: Maybe SyncStrategy
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { configUrl :: Maybe String
    , configUsername :: Maybe Username
    , configCacheDir :: Maybe FilePath
    , configDataDir :: Maybe FilePath
    , configSyncStrategy :: Maybe SyncStrategy
    }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON =
    withObject "Configuration" $ \o ->
      Configuration <$> o .:? "url" <*> o .:? "username" <*> o .:? "cache-dir" <*> o .:? "data-dir" <*>
      o .:? "sync"

data Settings =
  Settings
    { setBaseUrl :: Maybe BaseUrl
    , setUsername :: Maybe Username
    , setCacheDir :: Path Abs Dir
    , setDataDir :: Path Abs Dir
    , setSyncStrategy :: SyncStrategy
    }
  deriving (Show, Eq, Generic)

data SyncStrategy
  = NeverSync
  | AlwaysSync
  deriving (Show, Eq, Generic)

instance FromJSON SyncStrategy

instance ToJSON SyncStrategy

data Dispatch
  = DispatchRegister RegisterSettings
  | DispatchLogin LoginSettings
  | DispatchPostPostAddItem Text
  | DispatchShowItem
  | DispatchDoneItem
  | DispatchSize
  | DispatchReview
  | DispatchLogout
  | DispatchSync
  deriving (Show, Eq, Generic)

data RegisterSettings =
  RegisterSettings
    { registerSetUsername :: Maybe Username
    , registerSetPassword :: Maybe Text
    }
  deriving (Show, Eq, Generic)

data LoginSettings =
  LoginSettings
    { loginSetUsername :: Maybe Username
    , loginSetPassword :: Maybe Text
    }
  deriving (Show, Eq, Generic)

type CliM = ReaderT Settings IO
