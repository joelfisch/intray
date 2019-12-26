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
    genValidSpec @Registration
    jsonSpecOnValid @Registration
    genValidSpec @LoginForm
    jsonSpecOnValid @LoginForm
