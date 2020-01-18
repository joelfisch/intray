{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Review
  ( review
  ) where

import Import

import Intray.Cli.Commands.Done
import Intray.Cli.Commands.Show
import Intray.Cli.OptParse
import Intray.Cli.Prompt
import Intray.Cli.Store
import Intray.Cli.Sync

review :: CliM ()
review = do
  showItem
  res <- liftIO $ prompt "done [y/N]"
  let showSize = do
        s <- syncAndReturn storeSize
        liftIO $ putStrLn $ unwords [show s, "items remaining"]
  let cont = do
        doneItem
        showSize
        review
      stop = pure ()
  case res of
    "y" -> cont
    "Y" -> cont
    "n" -> stop
    "N" -> stop
    _ -> do
      showSize
      review
