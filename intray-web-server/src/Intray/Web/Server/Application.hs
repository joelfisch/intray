{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Web.Server.Application where

import Yesod
import Yesod.Auth

import Intray.Web.Server.Foundation
import Intray.Web.Server.Handler

mkYesodDispatch "App" resourcesApp
