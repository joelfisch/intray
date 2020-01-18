{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected.Account
  ( IntrayProtectedAccountAPI
  , IntrayProtectedAccountSite(..)
  , AuthCookie(..)
  , AccountInfo(..)
  , GetAccountInfo
  , DeleteAccount
  ) where

import Import

import Servant.API
import Servant.Auth.Docs ()
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Generic

import Intray.API.Protected.Account.Types
import Intray.API.Types

type IntrayProtectedAccountAPI = ToServant (IntrayProtectedAccountSite AsApi)

data IntrayProtectedAccountSite route =
  IntrayProtectedAccountSite
    { getAccountInfo :: !(route :- GetAccountInfo)
    , deleteAccount :: !(route :- DeleteAccount)
    }
  deriving (Generic)

type GetAccountInfo = ProtectAPI :> Get '[ JSON] AccountInfo

type DeleteAccount = ProtectAPI :> Delete '[ JSON] NoContent
