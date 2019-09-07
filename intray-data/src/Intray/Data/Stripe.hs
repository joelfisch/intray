-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Intray.Data.Stripe where

import Import

import Web.Stripe.Types as Stripe

import Database.Persist
import Database.Persist.Sql

deriving instance PersistField CustomerId

deriving instance PersistFieldSql CustomerId
