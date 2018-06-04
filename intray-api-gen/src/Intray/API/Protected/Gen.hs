{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Intray.API.Protected.Gen
    ( module Intray.API.Protected.Item.Gen
    , module Intray.API.Protected.Account.Gen
    ) where

import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()

import Intray.Data.Gen ()

import Intray.API.Protected.Account.Gen ()
import Intray.API.Protected.Item.Gen ()