{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Web.Server.DB where

import Database.Persist.Sql
import Database.Persist.TH
import Intray.Client
import Intray.Web.Server.Persistence ()
import Servant.Auth.Client (Token(..))

share
  [mkPersist sqlSettings, mkMigrate "migrateLoginCache"]
  [persistLowerCase|

UserToken
    name Username
    token Token

    UniqueUserToken name

    deriving Show
    deriving Eq
    deriving Generic
|]
