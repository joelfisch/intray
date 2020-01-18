{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Error
  ( getErrorAPIDownR
  ) where

import Import

import qualified Data.Text as T
import Text.Read (readMaybe)

import Yesod

import Intray.Web.Server.Foundation

getErrorAPIDownR :: Text -> Handler Html
getErrorAPIDownR e = do
  let ms = readMaybe (T.unpack e) :: Maybe String
  withNavBar $(widgetFile "api-down")
