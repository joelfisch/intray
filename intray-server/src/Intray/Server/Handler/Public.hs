module Intray.Server.Handler.Public
    ( IntrayPublicSite
    , intrayPublicServer
    ) where

import Servant.Generic

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Public.GetDocs
import Intray.Server.Handler.Public.PostLogin
import Intray.Server.Handler.Public.PostRegister

intrayPublicServer :: IntrayPublicSite (AsServerT IntrayHandler)
intrayPublicServer =
    IntrayPublicSite
    { postRegister = servePostRegister
    , postLogin = servePostLogin
    , getDocs = serveGetDocs
    }
