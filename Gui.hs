module Main where

import qualified School.Gui as Gui
import Paths_school(getDataFileName)

main :: IO ()
main = Gui.main =<< getDataFileName "school.glade"
