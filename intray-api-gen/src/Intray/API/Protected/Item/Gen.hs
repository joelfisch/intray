{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Intray.API.Protected.Item.Gen where

import Import

import Intray.Data.Gen ()

import Intray.API.Protected

instance GenValid TypedItem where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid a => GenValid (ItemInfo a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
