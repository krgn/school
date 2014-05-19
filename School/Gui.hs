module School.Gui where

import System.IO
import System.IO.Error
import Network.Socket
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Control.Concurrent
import Data.IORef
import School.Config
import Control.Monad(void)
import Control.Exception
import Prelude hiding (catch)
import Text.Printf
import Control.Monad

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

        statusView :: Label,
        progBar :: ProgressBar
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
       pBar   <- xmlGetWidget xml castToProgressBar "showProgress"

       return $ GUI mw pBtn sBtn rBtn jtsBtn jtmBtn jteBtn qBtn aBtn sView pBar


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
    onActivateItem (quitBtn $ appGui app) mainQuit

    -- onActivateItem (aboutBtn $ appGui app) $ do 
    --     dialog <- aboutDialogNew 
    --     return ()
 
    onClicked (playBtn $ appGui app) $ do
        thId <- forkIO $ waitingTask osd 0 app
        writeIORef thread (Just thId)
        
    onClicked (stopBtn $ appGui app) $ do
        forkIO $ stopAll hosts
        thId <- readIORef thread
        case thId of
            Nothing -> putStrLn "oh, no ThreadId found!"
            Just i -> killThread i

    onClicked (verboseBtn $ appGui app) $
        void $ forkIO $ do 
            toggleOsd osd
            showTimecode osd hosts

    onClicked (jumpToStartBtn $ appGui app) $
        void $ forkIO $ seekTo 0 hosts
            
    onClicked (jumpToMiddleBtn $ appGui app) $
        void $ forkIO $ seekTo 50 hosts
            
    onClicked (jumpToEndBtn $ appGui app) $ 
        void $ forkIO $ seekTo 99 hosts
            
    return ()

    where 
        waitingTask :: IORef Bool -> Int -> App -> IO ()
        waitingTask o i a = do 
            let hosts = getHosts $ appCfg a
            let duration = fromIntegral $ getDuration $ appCfg a

            postGUIAsync $ updateTimecode i a
            postGUIAsync $ updateProgrss i a

            when (i == 0) $ do 
                startAll hosts
                showTimecode o hosts

            threadDelay 1000000
            waitingTask o ((i + 1) `mod` duration) a


updateProgrss :: Int -> App -> IO ()
updateProgrss i app = do
    let bar = progBar (appGui app)
    let duration = fromIntegral $ getDuration (appCfg app)
    let total = (1.0 /  duration) * (fromIntegral i)

    putStrLn $ show total
    progressBarSetFraction bar total


updateTimecode :: Int -> App -> IO ()
updateTimecode i app = do
    let label = statusView (appGui app)
    labelSetMarkup label str
    
    where
        hours = i `div` 60 `div` 60
        minutes = i `div` 60
        seconds = i `mod` 60
        str = concat [ 
            "<span font='50' font_weight='heavy'>"
            , printf "%02d:%02d:%02d" hours minutes seconds 
            , "</span>" ]


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

    void $ forkIO $ 
        catchIOError (connAndSend sock (addrAddress servAddr)) handler

    where   
        handler e = putStrLn "error!" >> return ()

        connAndSend sock addr = do
            connect sock addr
            hock <- socketToHandle sock WriteMode
            -- set to block buffering, as we use flush to get out our message asap
            hSetBuffering hock (BlockBuffering Nothing)

            hPutStrLn hock msg
            hFlush hock

            hClose hock

