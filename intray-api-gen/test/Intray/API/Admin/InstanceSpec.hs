{-# LANGUAGE TypeApplications #-}

module Intray.API.Admin.InstanceSpec
  ( spec
  ) where

import TestImport

import Test.Validity.Aeson

import Intray.API.Admin.Types

import Intray.API.Admin.Gen ()

spec :: Spec
spec = do
  genValidSpec @AdminStats
  jsonSpecOnValid @AdminStats
