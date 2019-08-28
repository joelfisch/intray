module Intray.Server.Types
  ( module Intray.Server.Types
  , MonetisationSettings(..)
  ) where

import Import

import Database.Persist.Sqlite

import Servant
import Servant.Auth.Server

import Intray.API

import Intray.Server.OptParse.Types

data IntrayServerEnv =
  IntrayServerEnv
    { envConnectionPool :: !ConnectionPool
    , envCookieSettings :: !CookieSettings
    , envJWTSettings :: !JWTSettings
    , envAdmins :: ![Username]
    , envMonetisationSettings :: !(Maybe MonetisationSettings)
    }

type IntrayHandler = ReaderT IntrayServerEnv Handler
