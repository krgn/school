module Main where

import System.Environment
import qualified School.Client as Client
import qualified School.Gui as Gui
import Paths_school(getDataFileName)

main :: IO ()
main = do 
    args <- getArgs
    case filter (== "--master") args of 
        [] -> Client.main
        _  -> do gladefn <- getDataFileName "school.glade"
                 Gui.main gladefn
