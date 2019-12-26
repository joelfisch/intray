module Intray.Cli.Path
  ( sessionPath
  , lastSeenItemPath
  , storePath
  ) where

import Import

import Intray.Cli.OptParse

intrayDir :: CliM (Path Abs Dir)
intrayDir = asks setIntrayDir

sessionPath :: CliM (Path Abs File)
sessionPath = do
  d <- intrayDir
  resolveFile d "session.cookie"

lastSeenItemPath :: CliM (Path Abs File)
lastSeenItemPath = do
  d <- intrayDir
  resolveFile d "last-seen-item.json"

storePath :: CliM (Path Abs File)
storePath = do
  d <- intrayDir
  resolveFile d "store.json"
