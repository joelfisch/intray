{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Server.Handler.Public.GetDocs
  ( serveGetDocs
  ) where

import Import

import Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Text.Markdown as Markdown

import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Docs as Docs

import Intray.API

import Intray.Server.Types

serveGetDocs :: IntrayHandler GetDocsResponse
serveGetDocs = do
  host <- asks envHost
  pure $ intrayHtmlResponse host

intrayHtmlResponse :: Text -> GetDocsResponse
intrayHtmlResponse host =
  GetDocsResponse $
  Markdown.markdown Markdown.defaultMarkdownSettings {Markdown.msXssProtect = False} $
  LT.fromStrict $ intrayDocs host

intrayDocs :: Text -> Text
intrayDocs host =
  T.unlines .
  map
    (\t ->
       if T.isPrefixOf "```" (T.stripStart t)
         then T.stripStart t
         else t) .
  T.lines . T.pack $
  Docs.markdown $ Docs.docsWithIntros [intr] $ Docs.pretty intrayOpenAPI
  where
    intr =
      Docs.DocIntro
        "Intray API"
        [ unlines
            ["<style>", T.unpack $ TE.decodeUtf8 $(embedFile "res/style/docs.css"), "</style>"]
        , "Please find the api endpoints at " <> T.unpack host <> "."
        ]
