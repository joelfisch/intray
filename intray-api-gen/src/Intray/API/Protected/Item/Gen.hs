{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Intray.API.Protected.Item.Gen where

import Import

import Intray.Data.Gen ()

import Intray.API.Protected

instance GenUnchecked TypedItem

instance GenValid TypedItem where
    genValid = genValidStructurally

instance GenUnchecked a => GenUnchecked (ItemInfo a)

instance GenValid a => GenValid (ItemInfo a) where
    genValid = genValidStructurally
