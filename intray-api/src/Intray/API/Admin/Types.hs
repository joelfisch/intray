{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.API.Admin.Types where

import Import

import Data.Aeson as JSON

import Servant.Docs

import Intray.API.Types ()

data AdminStats =
  AdminStats
    { adminStatsNbAccounts :: !Word
    , adminStatsNbItems :: !Word
    , adminStatsActiveUsers :: !ActiveUsers
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity AdminStats

instance FromJSON AdminStats where
  parseJSON =
    withObject "AdminStats" $ \o ->
      AdminStats <$> o .: "accounts" <*> o .: "items" <*> o .: "active-users"

instance ToJSON AdminStats where
  toJSON AdminStats {..} =
    object
      [ "accounts" .= adminStatsNbAccounts
      , "items" .= adminStatsNbItems
      , "active-users" .= adminStatsActiveUsers
      ]

instance ToSample AdminStats

data ActiveUsers =
  ActiveUsers
    { activeUsersDaily :: !Word
    , activeUsersWeekly :: !Word
    , activeUsersMonthly :: !Word
    , activeUsersYearly :: !Word
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity ActiveUsers

instance FromJSON ActiveUsers where
  parseJSON =
    withObject "ActiveUsers" $ \o ->
      ActiveUsers <$> o .: "daily" <*> o .: "weekly" <*> o .: "monthly" <*> o .: "yearly"

instance ToJSON ActiveUsers where
  toJSON ActiveUsers {..} =
    object
      [ "daily" .= activeUsersDaily
      , "weekly" .= activeUsersWeekly
      , "monthly" .= activeUsersMonthly
      , "yearly" .= activeUsersYearly
      ]

instance ToSample ActiveUsers
