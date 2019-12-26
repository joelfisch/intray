module Intray.Data.AccessKeyUUID
  ( AccessKeyUUID
  , module Data.UUID.Typed
  ) where

import Data.UUID.Typed

import Intray.Data.UUID ()

type AccessKeyUUID = UUID AccessKey

data AccessKey
