{-# LANGUAGE TypeApplications #-}

module Intray.API.InstanceSpec
  ( spec
  ) where

import TestImport

import Test.Validity.Aeson

import Intray.API.Gen ()
import Intray.API.Types

spec :: Spec
spec = do
  eqSpec @Permission
  ordSpec @Permission
  genValidSpec @Permission
  jsonSpecOnValid @Permission
  eqSpecOnValid @Registration
  ordSpecOnValid @Registration
  genValidSpec @Registration
  jsonSpecOnValid @Registration
  eqSpecOnValid @LoginForm
  ordSpecOnValid @LoginForm
  genValidSpec @LoginForm
  jsonSpecOnValid @LoginForm
  eqSpecOnValid @Pricing
  genValidSpec @Pricing
  jsonSpecOnValid @Pricing
