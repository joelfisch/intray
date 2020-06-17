{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Web.Server.Persistence where

import Database.Persist.Sql
import Servant.Auth.Client (Token(..))

deriving instance PersistField Token

deriving instance PersistFieldSql Token
