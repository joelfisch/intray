{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Data.AccessKeySecret
  ( AccessKeySecret
  , generateRandomAccessKeySecret
  , accessKeySecretText
  , parseAccessKeySecretText
  ) where

import Intray.Data.Import

import Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base16 as SB16
import qualified Data.Text.Encoding as TE
import System.Random

import Database.Persist
import Database.Persist.Sql

newtype AccessKeySecret =
  AccessKeySecret ByteString
  deriving (Show, Eq, Ord, Generic, PersistField, PersistFieldSql)

instance Validity AccessKeySecret

instance FromJSON AccessKeySecret where
  parseJSON =
    withText "AccessKeySecret" $ \t ->
      case parseAccessKeySecretText t of
        Nothing -> fail "Invalid AccessKeySecret"
        Just aks -> pure aks

instance ToJSON AccessKeySecret where
  toJSON = toJSON . accessKeySecretText

accessKeySecretText :: AccessKeySecret -> Text
accessKeySecretText (AccessKeySecret bs) = TE.decodeUtf8 $ SB16.encode bs

parseAccessKeySecretText :: Text -> Maybe AccessKeySecret
parseAccessKeySecretText t =
  case SB16.decode $ TE.encodeUtf8 t of
    (d, "") -> Just $ AccessKeySecret d
    _ -> Nothing

generateRandomAccessKeySecret :: IO AccessKeySecret
generateRandomAccessKeySecret = AccessKeySecret . SB.pack <$> replicateM 16 randomIO
