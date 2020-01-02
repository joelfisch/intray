{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Looper
  ( LoopersSettings(..)
  , runIntrayServerLoopers
  ) where

import Import

import Control.Monad.Logger
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sqlite

import Looper

import Intray.Server.Looper.Import
import Intray.Server.Looper.StripeEventsFetcher
import Intray.Server.OptParse.Types

data LoopersSettings =
  LoopersSettings
    { loopersSetLogLevel :: !LogLevel
    , loopersSetConnectionPool :: !ConnectionPool
    , loopersSetStripeSettings :: !StripeSettings
    , loopersSetStripeEventsFetcher :: !LooperSettings
    , loopersSetStripeEventsRetrier :: !LooperSettings
    }
  deriving (Show)

runIntrayServerLoopers :: LoopersSettings -> IO ()
runIntrayServerLoopers LoopersSettings {..} =
  let env =
        LooperEnv
          { looperEnvStripeSettings = loopersSetStripeSettings
          , looperEnvConnectionPool = loopersSetConnectionPool
          }
   in flip runReaderT env $
      runStderrLoggingT $
      filterLogger (\_ ll -> ll >= loopersSetLogLevel) $
      runLoopersIgnoreOverrun customRunner looperDefs
  where
    customRunner ld = do
      logDebugNS (looperDefName ld) "Starting"
      start <- liftIO getCurrentTime
      looperDefFunc ld
      end <- liftIO getCurrentTime
      logDebugNS (looperDefName ld) $ "Done, took " <> T.pack (show (diffUTCTime end start))
    looperDefs =
      [ mkLooperDef "stripe-events-fetcher" loopersSetStripeEventsFetcher stripeEventsFetcherLooper
      , mkLooperDef "stripe-events-retrier" loopersSetStripeEventsRetrier (pure ())
      ]
