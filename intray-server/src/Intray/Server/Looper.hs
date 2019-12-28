{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Looper
  ( LoopersSettings(..)
  , runIntrayServerLoopers
  ) where

import Import

import Looper

data LoopersSettings =
  LoopersSettings
    { loopersSetStripeEventsFetcher :: LooperSettings
    , loopersSetStripeEventsRetrier :: LooperSettings
    }
  deriving (Show)

runIntrayServerLoopers :: LoopersSettings -> IO ()
runIntrayServerLoopers LoopersSettings {..} =
  runLoopers
    [ mkLooperDef "stripe-events-fetcher" loopersSetStripeEventsFetcher (pure ())
    , mkLooperDef "stripe-events-retrier" loopersSetStripeEventsRetrier (pure ())
    ]
