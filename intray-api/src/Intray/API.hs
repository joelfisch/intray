{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API
  ( module Intray.Data
  , module Intray.API
  , module Intray.API.Admin
  , module Intray.API.Protected
  , module Intray.API.Types
  , module Data.UUID.Typed
  ) where

import Import

import Data.UUID.Typed

import Web.Cookie

import Servant.API
import Servant.Auth.Docs ()
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Generic
import Servant.HTML.Blaze

import Intray.Data

import Intray.API.Admin
import Intray.API.Protected
import Intray.API.Types

intrayAPI :: Proxy IntrayAPI
intrayAPI = Proxy

type IntrayAPI = ToServant (IntraySite AsApi)

data IntraySite route =
  IntraySite
    { openSite :: !(route :- ToServant (IntrayOpenSite AsApi))
    , adminSite :: !(route :- "admin" :> ToServant (IntrayAdminSite AsApi))
    }
  deriving (Generic)

intrayOpenAPI :: Proxy IntrayOpenAPI
intrayOpenAPI = Proxy

type IntrayOpenAPI = ToServant (IntrayOpenSite AsApi)

data IntrayOpenSite route =
  IntrayOpenSite
    { protectedSite :: !(route :- ToServant (IntrayProtectedSite AsApi))
    , publicSite :: !(route :- ToServant (IntrayPublicSite AsApi))
    }
  deriving (Generic)

type IntrayPublicAPI = ToServant (IntrayPublicSite AsApi)

data IntrayPublicSite route =
  IntrayPublicSite
    { postRegister :: !(route :- PostRegister)
    , postLogin :: !(route :- PostLogin)
    , getDocs :: !(route :- GetDocs)
    , getPricing :: !(route :- GetPricing)
    }
  deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type PostRegister = "register" :> ReqBody '[ JSON] Registration :> Post '[ JSON] NoContent

type PostLogin
   = "login" :> ReqBody '[ JSON] LoginForm :> PostNoContent '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type GetDocs = Get '[ HTML] GetDocsResponse

type GetPricing = "pricing" :> Get '[ JSON] (Maybe Pricing)
