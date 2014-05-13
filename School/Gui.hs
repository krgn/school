module School.Gui where

import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
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
    thid <- forkIO (task 10)
    yield

    where task n = do
            putStrLn $ "hello " ++ show n
            threadDelay 1000000
            if n > 0
                then task (n - 1)
                else do thid <- myThreadId
                        killThread thid
        
