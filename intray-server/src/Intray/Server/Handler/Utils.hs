{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.Utils
  ( runDb
  , deleteAccountFully
  ) where

import Import

import Database.Persist
import Database.Persist.Sqlite

import Servant

import Intray.API

import Intray.Server.Types

runDb :: (MonadReader IntrayServerEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool query pool

deleteAccountFully :: AccountUUID -> IntrayHandler ()
deleteAccountFully uuid = do
  mEnt <- runDb $ getBy $ UniqueUserIdentifier uuid
  case mEnt of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity uid _) ->
      runDb $ do
        deleteWhere [IntrayItemUserId ==. uuid]
        delete uid
