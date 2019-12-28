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
import Looper

data LoopersSettings =
  LoopersSettings
    { loopersSetLogLevel :: LogLevel
    , loopersSetStripeEventsFetcher :: LooperSettings
    , loopersSetStripeEventsRetrier :: LooperSettings
    }
  deriving (Show)

runIntrayServerLoopers :: LoopersSettings -> IO ()
runIntrayServerLoopers LoopersSettings {..} =
  runStderrLoggingT $
  filterLogger (\_ ll -> ll >= loopersSetLogLevel) $ runLoopersIgnoreOverrun customRunner looperDefs
  where
    customRunner ld = do
      logDebugNS (looperDefName ld) "Starting"
      start <- liftIO getCurrentTime
      looperDefFunc ld
      end <- liftIO getCurrentTime
      logDebugNS (looperDefName ld) $ "Done, took " <> T.pack (show (diffUTCTime end start))
    looperDefs =
      [ mkLooperDef "stripe-events-fetcher" loopersSetStripeEventsFetcher (pure ())
      , mkLooperDef "stripe-events-retrier" loopersSetStripeEventsRetrier (pure ())
      ]
