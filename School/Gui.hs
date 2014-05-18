module School.Gui where

import System.IO
import Network.Socket
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Control.Concurrent
import Data.IORef
import School.Config
import Control.Monad(void)

data App = App {
        appGui :: GUI,
        appCfg :: SchoolConfig
    }

data GUI = GUI {
        mainWin :: Window, 
        playBtn :: Button,
        stopBtn :: Button,
        verboseBtn :: Button,
        jumpToStartBtn :: Button,
        jumpToMiddleBtn :: Button,
        jumpToEndBtn :: Button,

        quitBtn :: MenuItem,
        aboutBtn :: MenuItem,

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


loadGlade :: FilePath -> IO GUI
loadGlade gladepath = 
    do Just xml <- xmlNew gladepath
       -- the main window
       mw     <- xmlGetWidget xml castToWindow "mainWindow"
       pBtn   <- xmlGetWidget xml castToButton "playButton"
       sBtn   <- xmlGetWidget xml castToButton "stopButton"
       rBtn   <- xmlGetWidget xml castToButton "verboseButton"
       jtsBtn <- xmlGetWidget xml castToButton "jumpToStart"
       jtmBtn <- xmlGetWidget xml castToButton "jumpToMiddle"
       jteBtn <- xmlGetWidget xml castToButton "jumpToEnd"
       qBtn   <- xmlGetWidget xml castToMenuItem "quitButton"
       aBtn   <- xmlGetWidget xml castToMenuItem "aboutDialog"
       sView  <- xmlGetWidget xml castToLabel "statusView"

       return $ GUI mw pBtn sBtn rBtn jtsBtn jtmBtn jteBtn qBtn aBtn sView


threadRef :: IO (IORef (Maybe ThreadId))
threadRef = newIORef Nothing

osdRef :: IO (IORef Bool)
osdRef = newIORef False

connectGui :: App -> IO () 
connectGui app = do 
    thread <- threadRef 
    osd <- osdRef

    let hosts = getHosts $ appCfg app

    onDestroy (mainWin $ appGui app) mainQuit
    -- onActivateItem (quitBtn $ appGui app) mainQuit
    -- onActivateItem (aboutBtn $ appGui app) $ do 
    --     dialog <- aboutDialogNew 
    --     return ()
 
    onClicked (playBtn $ appGui app) $ do
        thId <- forkIO $ waitingTask app
        writeIORef thread (Just thId)
        
    onClicked (stopBtn $ appGui app) $ do
        forkIO $ stopAll hosts
        thId <- readIORef thread
        case thId of
            Nothing -> putStrLn "oh, no ThreadId found!"
            Just i -> killThread i

    onClicked (verboseBtn $ appGui app) $
        void $ forkIO $ showTimecode osd hosts

    onClicked (jumpToStartBtn $ appGui app) $
        void $ forkIO $ seekTo 0 hosts
            
    onClicked (jumpToMiddleBtn $ appGui app) $
        void $ forkIO $ seekTo 50 hosts
            
    onClicked (jumpToEndBtn $ appGui app) $ 
        void $ forkIO $ seekTo 99 hosts
            
    return ()

    where 
        waitingTask :: App -> IO ()
        waitingTask a = do
            let hosts = getHosts $ appCfg a
            let duration = getDuration $ appCfg a

            startAll hosts

            threadDelay (1000000 * fromIntegral duration)
            waitingTask a


startAll :: [Host] -> IO ()
startAll = mapM_ (void . start) 
    where 
        start host = do
            let cmd = "loadfile " ++ getFilePath host
            void $ sendMessage cmd host


stopAll :: [Host] -> IO ()
stopAll = mapM_ (void . stop) 
    where 
        stop host = do
            let cmd = "stop"
            void $ sendMessage cmd host


seekTo :: Integer -> [Host] -> IO ()
seekTo p = mapM_ (void . seek p)
    where 
        seek p host = do
            let cmd = "seek " ++ show p ++ " 1"
            void $ sendMessage cmd host

showTimecode :: IORef Bool -> [Host] -> IO ()
showTimecode t hosts = do
    toggleOsd t
    osd <- readIORef t
    mapM_ (void . timecode osd) hosts
    where 
        timecode t host = do
            let cmd = if t then "osd 3" else "osd 0"
            void $ sendMessage cmd host

toggleOsd :: IORef Bool -> IO ()
toggleOsd ref = do
    osd <- readIORef ref
    writeIORef ref (not osd)

sendMessage :: String -> Host -> IO ()
sendMessage msg host = withSocketsDo $ do
    addrInfos <- getAddrInfo Nothing (Just $ getIP host) (Just $ getPort host)
    let servAddr = head addrInfos

    sock <- socket (addrFamily servAddr) Stream defaultProtocol

    connect sock (addrAddress servAddr) 

    hock <- socketToHandle sock WriteMode
    -- set to block buffering, as we use flush to get out our message asap
    hSetBuffering hock (BlockBuffering Nothing)

    hPutStrLn hock msg
    hFlush hock

    hClose hock

