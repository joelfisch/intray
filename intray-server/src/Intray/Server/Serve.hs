module Intray.Server.Serve
  ( intrayServer
  ) where

import Servant.Generic

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler

intrayServer :: IntraySite (AsServerT IntrayHandler)
intrayServer =
  IntraySite {openSite = toServant intrayOpenServer, adminSite = toServant intrayAdminServer}

intrayOpenServer :: IntrayOpenSite (AsServerT IntrayHandler)
intrayOpenServer =
  IntrayOpenSite
    {protectedSite = toServant intrayProtectedServer, publicSite = toServant intrayPublicServer}

intrayProtectedServer :: IntrayProtectedSite (AsServerT IntrayHandler)
intrayProtectedServer =
  IntrayProtectedSite
    { protectedItemSite = toServant intrayProtectedItemServer
    , protectedAccountSite = toServant intrayProtectedAccountServer
    , protectedAccessKeySite = toServant intrayProtectedAccessKeyServer
    , getPermissions = serveGetPermissions
    }

intrayProtectedItemServer :: IntrayProtectedItemSite (AsServerT IntrayHandler)
intrayProtectedItemServer =
  IntrayProtectedItemSite
    { getShowItem = serveGetShowItem
    , getIntraySize = serveGetIntraySize
    , getItemUUIDs = serveGetItemUUIDs
    , getItems = serveGetItems
    , postAddItem = servePostAddItem
    , getItem = serveGetItem
    , deleteItem = serveDeleteItem
    , postSync = servePostSync
    }

intrayProtectedAccountServer :: IntrayProtectedAccountSite (AsServerT IntrayHandler)
intrayProtectedAccountServer =
  IntrayProtectedAccountSite
    {getAccountInfo = serveGetAccountInfo, deleteAccount = serveDeleteAccount}

intrayProtectedAccessKeyServer :: IntrayProtectedAccessKeySite (AsServerT IntrayHandler)
intrayProtectedAccessKeyServer =
  IntrayProtectedAccessKeySite
    { postAddAccessKey = servePostAddAccessKey
    , getAccessKey = serveGetAccessKey
    , getAccessKeys = serveGetAccessKeys
    , deleteAccessKey = serveDeleteAccessKey
    }

intrayAdminServer :: IntrayAdminSite (AsServerT IntrayHandler)
intrayAdminServer =
  IntrayAdminSite
    { adminGetStats = serveAdminGetStats
    , adminDeleteAccount = serveAdminDeleteAccount
    , adminGetAccounts = serveAdminGetAccounts
    }

intrayPublicServer :: IntrayPublicSite (AsServerT IntrayHandler)
intrayPublicServer =
  IntrayPublicSite
    { postRegister = servePostRegister
    , postLogin = servePostLogin
    , getDocs = serveGetDocs
    , getPricing = serveGetPricing
    }
