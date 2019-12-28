module Intray.Server.Looper.Import
  ( module Intray.Server.Looper.Import
  , module X
  ) where

import Import as X

import Control.Monad.Logger as X

import Looper as X

import Intray.Server.OptParse.Types
import Intray.Server.Stripe

type Looper = LoggingT (ReaderT LooperEnv IO)

data LooperEnv =
  LooperEnv
    { looperEnvStripeSettings :: !StripeSettings
    }
  deriving (Show)
