module Intray.Cli.TestUtils
  ( intray
  ) where

import TestImport

import Intray.Cli (intrayCli)

intray :: [String] -> IO ()
intray args = do
  putStrLn $ unwords $ "RUNNING:" : "intray" : args
  withArgs args intrayCli
