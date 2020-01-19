module Intray.Server.Serve
  ( intrayServer
  ) where

import Data.Set (Set)
import qualified Data.Set as S

import Servant.Auth.Server
import Servant.Generic
import Servant.Server

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
    , getPermissions = withAuthResultAndPermission PermitGetPermissions serveGetPermissions
    }

intrayProtectedItemServer :: IntrayProtectedItemSite (AsServerT IntrayHandler)
intrayProtectedItemServer =
  IntrayProtectedItemSite
    { getShowItem = withAuthResultAndPermission PermitShow serveGetShowItem
    , getIntraySize = withAuthResultAndPermission PermitSize serveGetIntraySize
    , getItemUUIDs = withAuthResultAndPermission PermitGetItemUUIDs serveGetItemUUIDs
    , getItems = withAuthResultAndPermission PermitGetItems serveGetItems
    , postAddItem = withAuthResultAndPermission PermitAdd servePostAddItem
    , getItem = withAuthResultAndPermission PermitGetItem serveGetItem
    , deleteItem = withAuthResultAndPermission PermitDelete serveDeleteItem
    , postSync = withAuthResultAndPermission PermitSync servePostSync
    }

intrayProtectedAccountServer :: IntrayProtectedAccountSite (AsServerT IntrayHandler)
intrayProtectedAccountServer =
  IntrayProtectedAccountSite
    { getAccountInfo = withAuthResultAndPermission PermitGetAccountInfo serveGetAccountInfo
    , deleteAccount = withAuthResultAndPermission PermitDeleteAccount serveDeleteAccount
    }

intrayProtectedAccessKeyServer :: IntrayProtectedAccessKeySite (AsServerT IntrayHandler)
intrayProtectedAccessKeyServer =
  IntrayProtectedAccessKeySite
    { postAddAccessKey = withAuthResultAndPermission PermitPostAddAccessKey servePostAddAccessKey
    , getAccessKey = withAuthResultAndPermission PermitGetAccessKey serveGetAccessKey
    , getAccessKeys = withAuthResultAndPermission PermitGetAccessKeys serveGetAccessKeys
    , deleteAccessKey = withAuthResultAndPermission PermitDeleteAccessKey serveDeleteAccessKey
    }

intrayAdminServer :: IntrayAdminSite (AsServerT IntrayHandler)
intrayAdminServer =
  IntrayAdminSite
    { adminGetStats = withAuthResultAndPermission PermitAdminGetStats serveAdminGetStats
    , adminDeleteAccount =
        withAuthResultAndPermission PermitAdminDeleteAccount serveAdminDeleteAccount
    , adminGetAccounts = withAuthResultAndPermission PermitAdminGetAccounts serveAdminGetAccounts
    }

intrayPublicServer :: IntrayPublicSite (AsServerT IntrayHandler)
intrayPublicServer =
  IntrayPublicSite
    { postRegister = servePostRegister
    , postLogin = servePostLogin
    , getDocs = serveGetDocs
    , getPricing = serveGetPricing
    }

withAuthResult :: ThrowAll a => (AuthCookie -> a) -> (AuthResult AuthCookie -> a)
withAuthResult func ar =
  case ar of
    Authenticated ac -> func ac
    _ -> throwAll err401

withAuthResultAndPermission ::
     ThrowAll a => Permission -> (AuthCookie -> a) -> (AuthResult AuthCookie -> a)
withAuthResultAndPermission p func =
  withAuthResult (\ac -> withPermission (authCookiePermissions ac) p (func ac))

withPermission :: ThrowAll a => Set Permission -> Permission -> a -> a
withPermission ps p func =
  if S.member p ps
    then func
    else throwAll err401
