{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Server.Handler.Utils
  ( runDb
  , withPermission
  , deleteAccountFully
  ) where

import Import

import Data.Set (Set)
import qualified Data.Set as S

import Database.Persist
import Database.Persist.Sqlite

import Servant

import Intray.API
import Intray.Data

import Intray.Server.Types

runDb :: (MonadReader IntrayServerEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool query pool

withPermission :: Set Permission -> Permission -> IntrayHandler a -> IntrayHandler a
withPermission ps p func =
  if S.member p ps
    then func
    else throwError err401

deleteAccountFully :: AccountUUID -> IntrayHandler ()
deleteAccountFully uuid = do
  mEnt <- runDb $ getBy $ UniqueUserIdentifier uuid
  case mEnt of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity uid _) ->
      runDb $ do
        deleteWhere [IntrayItemUserId ==. uuid]
        delete uid
