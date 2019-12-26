{-# LANGUAGE TypeApplications #-}

module Intray.API.Protected.Item.InstanceSpec
  ( spec
  ) where

import TestImport

import Test.Validity.Aeson

import Intray.API.Protected.Item.Gen ()
import Intray.API.Protected.Item.Types

spec :: Spec
spec = do
  eqSpecOnValid @(ItemInfo ByteString)
  ordSpecOnValid @(ItemInfo ByteString)
  genValidSpec @(ItemInfo ByteString)
  eqSpecOnValid @TypedItem
  ordSpecOnValid @TypedItem
  genValidSpec @TypedItem
  jsonSpecOnValid @TypedItem
  eqSpecOnValid @(ItemInfo TypedItem)
  ordSpecOnValid @(ItemInfo TypedItem)
  genValidSpec @(ItemInfo TypedItem)
  jsonSpecOnValid @(ItemInfo TypedItem)
