module Intray.Cli
  ( intrayCli
  , dispatch
  ) where

import Import

import Intray.Cli.Commands.Add
import Intray.Cli.Commands.Done
import Intray.Cli.Commands.Login
import Intray.Cli.Commands.Logout
import Intray.Cli.Commands.Register
import Intray.Cli.Commands.Review
import Intray.Cli.Commands.Show
import Intray.Cli.Commands.Size
import Intray.Cli.Commands.Sync
import Intray.Cli.OptParse

intrayCli :: IO ()
intrayCli = do
  Instructions disp sett <- getInstructions
  runReaderT (dispatch disp) sett

dispatch :: Dispatch -> CliM ()
dispatch d =
  case d of
    DispatchRegister rs -> register rs
    DispatchLogin ls -> login ls
    DispatchPostPostAddItem t -> addItem t
    DispatchShowItem -> showItem
    DispatchDoneItem -> doneItem
    DispatchSize -> size
    DispatchReview -> review
    DispatchLogout -> logout
    DispatchSync -> sync
