{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Admin.Gen where

import Import

import Intray.API
import Intray.Data.Gen ()

instance GenValid AdminStats where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ActiveUsers where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
