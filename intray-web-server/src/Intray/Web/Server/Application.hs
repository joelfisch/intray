{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Web.Server.Application where

import Yesod
import Yesod.Auth

import Intray.Web.Server.Foundation
import Intray.Web.Server.Handler.APIDocs
import Intray.Web.Server.Handler.AccessKeys
import Intray.Web.Server.Handler.Account
import Intray.Web.Server.Handler.Add
import Intray.Web.Server.Handler.Admin
import Intray.Web.Server.Handler.Checkout
import Intray.Web.Server.Handler.Error
import Intray.Web.Server.Handler.Home
import Intray.Web.Server.Handler.Process

mkYesodDispatch "App" resourcesApp
