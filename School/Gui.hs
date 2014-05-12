module School.Gui where

import System.Environment 

import Data.Maybe 
import Network.Socket(withSocketsDo)

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade

import Control.Concurrent

data GUI = GUI {
        mainWin :: Window, 
        playBtn :: Button,
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
       pBtn <- xmlGetWidget xml castToButton "playButton"
       sView <- xmlGetWidget xml castToLabel "statusView"
       return $ GUI mw pBtn sView

connectGui gui = do 
        onDestroy (mainWin gui) mainQuit
        onClicked (playBtn gui) (startThread gui)

startThread gui = do
    thId <- forkIO (myTask 10)

    return ()
    where myTask n = do
              putStrLn $ show n 
              updateLabel $ show n
              threadDelay 200
              if n > 0
                  then myTask (n - 1)
                  else return ()
          updateLabel s = labelSetText (statusView gui) s
