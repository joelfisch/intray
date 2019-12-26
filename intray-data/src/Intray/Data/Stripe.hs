{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Data.Stripe where

import Web.Stripe.Types as Stripe

import Database.Persist
import Database.Persist.Sql

deriving instance PersistField CustomerId

deriving instance PersistFieldSql CustomerId
