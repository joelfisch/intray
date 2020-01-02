{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Looper.DB
  ( looperDB
  ) where

import Import

import Control.Exception

import Database.Persist.Sqlite (SqlPersistT, runSqlPool)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8

import Servant

import Web.Stripe as Stripe

import Intray.Server.Looper.Import
import Intray.Server.OptParse.Types
import Intray.Server.Stripe
import Intray.Server.Types

looperDB :: SqlPersistT IO b -> Looper b
looperDB query = do
  pool <- asks looperEnvConnectionPool
  liftIO $ runSqlPool query pool
