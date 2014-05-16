module School.Gui where

import Network.Socket(withSocketsDo)
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Control.Concurrent
import Data.IORef
import School.Config

data App = App {
        appGui :: GUI,
        appCfg :: SchoolConfig
    }

data GUI = GUI {
        mainWin :: Window, 
        playBtn :: Button,
        stopBtn :: Button,
        statusView :: Label
    }

main :: FilePath -> IO ()
main gladepath = do 
    -- GUI 
    initGUI

    gui <- loadGlade gladepath
    cfg <- readConfig

    connectGui $ App { appGui=gui, appCfg=cfg }

    widgetShowAll (mainWin gui)
    mainGUI


loadGlade gladepath = 
    do Just xml <- xmlNew gladepath
       -- the main window
       mw    <- xmlGetWidget xml castToWindow "mainWindow"
       pBtn  <- xmlGetWidget xml castToButton "playButton"
       sBtn  <- xmlGetWidget xml castToButton "stopButton"
       sView <- xmlGetWidget xml castToLabel "statusView"
       return $ GUI mw pBtn sBtn sView

threadRef :: IO (IORef (Maybe ThreadId))
threadRef = newIORef Nothing

connectGui :: App -> IO () 
connectGui app = do 
    thread <- threadRef 

    onDestroy (mainWin $ appGui app) mainQuit
    onClicked (playBtn $ appGui app) $ do
        thId <- forkIO $ uselessTask 0 
        writeIORef thread (Just thId)
        
    onClicked (stopBtn $ appGui app) $ do
        thId <- readIORef thread
        case thId of
            Nothing -> putStrLn "oh, not thread id found!"
            Just i -> killThread i

    return ()

    where uselessTask n = do
            putStrLn $ "hello " ++ show n
            threadDelay 1000000
            uselessTask $ n + 1
