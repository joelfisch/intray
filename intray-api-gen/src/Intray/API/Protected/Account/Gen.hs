{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Intray.API.Protected.Account.Gen where

import Import

import Intray.API
import Intray.Data.Gen ()

import Intray.API.Admin.Gen ()

instance GenValid AccountInfo where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally
