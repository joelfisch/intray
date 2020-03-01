{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Server.Handler.Public.GetSwagger
  ( serveGetSwagger
  ) where

import Import

import Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Text.Markdown as Markdown

import Data.Swagger (Swagger)
import Servant.Auth.Swagger as Servant
import Servant.Swagger as Servant

import Intray.API

import Intray.Server.Types

serveGetSwagger :: IntrayHandler Swagger
serveGetSwagger = do
  host <- asks envHost
  pure $ Servant.toSwagger intrayOpenAPI
