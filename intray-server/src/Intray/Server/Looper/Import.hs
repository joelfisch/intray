module Intray.Server.Looper.Import
  ( module Intray.Server.Looper.Import
  , module X
  ) where

import Import as X

import Database.Persist.Sqlite
import Control.Monad.Logger as X

import Looper as X

import Intray.Server.OptParse.Types

type Looper = LoggingT (ReaderT LooperEnv IO)

data LooperEnv =
  LooperEnv
    { looperEnvStripeSettings :: !StripeSettings
    , looperEnvConnectionPool :: !ConnectionPool
    }
  deriving (Show)
