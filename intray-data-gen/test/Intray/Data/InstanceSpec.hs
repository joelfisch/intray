{-# LANGUAGE TypeApplications #-}

module Intray.Data.InstanceSpec
  ( spec
  ) where

import TestImport

import Test.Validity.Aeson

import Intray.Data

import Intray.Data.Gen ()

spec :: Spec
spec = do
  jsonSpecOnValid @ItemUUID
  genValidSpec @ItemUUID
  jsonSpecOnValid @ItemType
  genValidSpec @ItemType
  genValidSpec @IntrayItem
  genValidSpec @Username
  jsonSpecOnValid @Username
  eqSpecOnValid @HashedPassword
  genValidSpec @HashedPassword
  genValidSpec @AccountUUID
  jsonSpecOnValid @AccountUUID
  genValidSpec @User
  genValidSpec @Permission
  jsonSpecOnValid @Permission
  genValidSpec @AccessKeySecret
  jsonSpecOnValid @AccessKeySecret
