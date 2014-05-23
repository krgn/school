module School.Gui where

import System.IO
import System.IO.Error
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Control.Concurrent
import Data.IORef
import School.Config
import School.Time
import School.Network
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
        refreshBtn :: MenuItem, 

        statusView :: Label,
        progBar :: ProgressBar
    }


main :: FilePath -> IO ()
main gladepath = do 
    -- GUI 
    _ <- initGUI

    gui <- loadGlade gladepath
    cfg <- readConfig

    connectGui App { appGui=gui, appCfg=cfg }

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
       reBtn   <- xmlGetWidget xml castToMenuItem "refreshButton"
       sView  <- xmlGetWidget xml castToLabel "statusView"
       pBar   <- xmlGetWidget xml castToProgressBar "showProgress"

       return  GUI { 
               mainWin           = mw
               , playBtn         = pBtn
               , stopBtn         = sBtn
               , verboseBtn      = rBtn
               , jumpToStartBtn  = jtsBtn
               , jumpToMiddleBtn = jtmBtn
               , jumpToEndBtn    = jteBtn
               , quitBtn         = qBtn
               , aboutBtn        = aBtn
               , refreshBtn      = reBtn
               , statusView      = sView
               , progBar         = pBar 
           }


threadRef :: IO (IORef (Maybe ThreadId))
threadRef = newIORef Nothing

osdRef :: IO (IORef Bool)
osdRef = newIORef False

appRef :: App -> IO (IORef App)
appRef = newIORef

connectGui :: App -> IO () 
connectGui a = do 
    thread <- threadRef 
    osd <- osdRef
    app <- appRef a

    -- or, liftM (getHosts . appCfg) (readIORef app)
    hosts <- readIORef app >>= return . getHosts . appCfg
    gui <- readIORef app >>= return . appGui

    onDestroy (mainWin gui) mainQuit

    on (quitBtn gui) menuItemActivate mainQuit

    on (refreshBtn gui) menuItemActivate $ do
        stop hosts thread
        curr <- readIORef app
        newCfg <- readConfig
        writeIORef app $ curr { appCfg=newCfg }

    onClicked (playBtn gui) $ do
        thId <- forkIO $ waitingTask osd 0 app
        writeIORef thread (Just thId)
        
    onClicked (stopBtn gui) $ stop hosts thread

    onClicked (verboseBtn gui) $
        void $ forkIO $ do 
            toggleOsd osd
            showTimecode osd hosts

    onClicked (jumpToStartBtn gui) $
        void $ forkIO $ seekTo 0 hosts
            
    onClicked (jumpToMiddleBtn gui) $
        void $ forkIO $ seekTo 50 hosts
            
    onClicked (jumpToEndBtn gui) $ 
        void $ forkIO $ seekTo 99 hosts
            
    return ()

    where 
        waitingTask :: IORef Bool -> Int -> IORef App -> IO ()
        waitingTask o i a = do 
            hosts <- liftM (getHosts . appCfg) (readIORef a)
            duration <- liftM (fromIntegral . getDuration . appCfg) (readIORef a)

            postGUIAsync $ updateTimecode i a
            postGUIAsync $ updateProgrss i a

            when (i == 0) $ do 
                startAll hosts
                showTimecode o hosts

            threadDelay 1000000
            waitingTask o ((i + 1) `mod` duration) a


stop :: [Host] -> IORef (Maybe ThreadId) -> IO ()
stop hosts thread = do 
    forkIO $ stopAll hosts
    thId <- readIORef thread
    case thId of
        Nothing -> putStrLn "oh, no ThreadId found!"
        Just i -> do killThread i
                     writeIORef thread Nothing 

updateProgrss :: Int -> IORef App -> IO ()
updateProgrss i app = do
    bar <- liftM (progBar . appGui) (readIORef app)
    duration <- liftM (fromIntegral . getDuration . appCfg) (readIORef app)
    let total = (1.0 /  duration) * fromIntegral i

    progressBarSetFraction bar total


updateTimecode :: Int -> IORef App -> IO ()
updateTimecode i app = do
    label <- liftM (statusView . appGui) (readIORef app)
    labelSetMarkup label str
    
    where
        hours = i `div` 60 `div` 60
        minutes = i `div` 60
        seconds = i `mod` 60
        str = concat [ 
            "<span font='50' font_weight='heavy'>"
            , printf "%02d:%02d:%02d" hours minutes seconds 
            , "</span>" ]


