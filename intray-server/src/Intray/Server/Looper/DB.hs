{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Server.Looper.DB
  ( looperDB
  ) where

import Import

import Database.Persist.Sqlite (SqlPersistT, runSqlPool)




import Intray.Server.Looper.Import

looperDB :: SqlPersistT IO b -> Looper b
looperDB query = do
  pool <- asks looperEnvConnectionPool
  liftIO $ runSqlPool query pool
