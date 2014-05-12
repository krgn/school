module Main where

import qualified SchoolGui
import Paths_school(getDataFileName)

main = 
    do gladefn <- getDataFileName "school.glade"
       SchoolGui.main gladefn
