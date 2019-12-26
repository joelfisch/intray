module Intray.Data.AccountUUID
  ( AccountUUID
  , module Data.UUID.Typed
  ) where

import Data.UUID.Typed

import Intray.Data.UUID ()

type AccountUUID = UUID User

data User
