module Intray.Server.Handler.Admin
    ( IntrayAdminSite
    , intrayAdminServer
    ) where

import Servant.Generic

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Admin.DeleteAccount
import Intray.Server.Handler.Admin.GetAccounts
import Intray.Server.Handler.Admin.GetStats

intrayAdminServer :: IntrayAdminSite (AsServerT IntrayHandler)
intrayAdminServer =
    IntrayAdminSite
    { adminGetStats = serveAdminGetStats
    , adminDeleteAccount = serveAdminDeleteAccount
    , adminGetAccounts = serveAdminGetAccounts
    }
