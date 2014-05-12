module Main where

import qualified School.Gui as Gui
import Paths_school(getDataFileName)

main :: IO ()
main = do 
    gladefn <- getDataFileName "school.glade"
    Gui.main gladefn
