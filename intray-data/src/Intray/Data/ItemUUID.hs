module Intray.Data.ItemUUID
  ( ItemUUID
  , module Data.UUID.Typed
  ) where

import Data.UUID.Typed

import Intray.Data.UUID ()

type ItemUUID = UUID Item

data Item
