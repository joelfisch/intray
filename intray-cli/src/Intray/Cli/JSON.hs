{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.JSON
  ( writeJSON
  , readJSON
  ) where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB
import Import

readJSON :: (MonadIO m, FromJSON a) => Path Abs File -> String -> m (Maybe a)
readJSON p extraErr =
  liftIO $ do
    mContents <- forgivingAbsence $ LB.readFile (toFilePath p)
    case mContents of
      Nothing -> pure Nothing
      Just "" -> pure Nothing
      Just contents ->
        case JSON.eitherDecode contents of
          Left err ->
            die $
            unlines
              [ unwords ["Unable to decode JSON file", fromAbsFile p, ", got error:", err]
              , ""
              , extraErr
              ]
          Right a -> pure a

writeJSON :: (MonadIO m, ToJSON a) => Path Abs File -> a -> m ()
writeJSON p a =
  liftIO $ do
    ensureDir $ parent p
    LB.writeFile (fromAbsFile p) (JSON.encodePretty a)
