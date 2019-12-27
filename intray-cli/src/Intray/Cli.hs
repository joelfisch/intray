module Intray.Cli
  ( intrayCli
  , dispatch
  ) where

import Import

import Intray.Cli.Commands
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
