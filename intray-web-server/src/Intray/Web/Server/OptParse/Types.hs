module Intray.Web.Server.OptParse.Types where

import Import

import Database.Persist.Sqlite

import Intray.API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandServe ServeFlags
    deriving (Show, Eq)

data ServeFlags = ServeFlags
    { serveFlagPort :: Maybe Int
    , serveFlagPersistLogins :: Maybe Bool
    , serveFlagTracking :: Maybe Text
    , serveFlagVerification :: Maybe Text
    , serveFlagAPIPort :: Maybe Int
    , serveFlagAPIDB :: Maybe Text
    , serveFlagAPIAdmins :: [String]
    } deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Environment = Environment
    { envPort :: Maybe Int
    , envAPIPort :: Maybe Int
    } deriving (Show, Eq)

newtype Dispatch =
    DispatchServe ServeSettings
    deriving (Show)

data ServeSettings = ServeSettings
    { serveSetPort :: Int
    , serveSetPersistLogins :: Bool
    , serveSetTracking :: Maybe Text
    , serveSetVerification :: Maybe Text
    , serveSetAPIPort :: Int
    , serveSetAPIConnectionInfo :: SqliteConnectionInfo
    , serveSetAPIAdmins :: [Username]
    } deriving (Show)

data Settings =
    Settings
    deriving (Show, Eq)
