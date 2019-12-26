{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Error
  ( getErrorAPIDownR
  ) where

import Import

import Yesod

import Intray.Web.Server.Foundation

getErrorAPIDownR :: Text -> Handler Html
getErrorAPIDownR e = withNavBar $(widgetFile "api-down")
