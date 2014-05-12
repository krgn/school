module SchoolGui where

import System.Environment 

import Data.Maybe 
import Network.Socket(withSocketsDo)

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade

import Control.Concurrent

data GUI = GUI {
        mainWin :: Window, 
        playBtn :: ToggleButton,
        statusView :: Label
    }

main :: FilePath -> IO ()
main gladepath = do 
    initGUI
    gui <- loadGlade gladepath
    connectGui gui
    widgetShowAll (mainWin gui)
    mainGUI

loadGlade gladepath = 
    do Just xml <- xmlNew gladepath
       -- the main window
       mw <- xmlGetWidget xml castToWindow "mainWindow"
       pBtn <- xmlGetWidget xml castToToggleButton "playButton"
       sView <- xmlGetWidget xml castToLabel "statusView"
       return $ GUI mw pBtn sView

connectGui gui = do 
        onDestroy (mainWin gui) mainQuit
        onClicked (playBtn gui) (setTextView gui)

setTextView gui = do
    labelSetText (statusView gui) "hello dear"
