module IntrayServer where

import Import

import Intray.Server (runIntrayServer)
import Intray.Server.OptParse
       (Dispatch(..), Settings(..), getInstructions)

intrayServer :: IO ()
intrayServer = do
    (DispatchServe serveSets, Settings) <- getInstructions
    putStrLn $ ppShow serveSets
    runIntrayServer serveSets
