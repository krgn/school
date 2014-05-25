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
import School.Types
import Text.Printf
import Control.Monad


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
       paBtn  <- xmlGetWidget xml castToButton "pauseButton"
       sBtn   <- xmlGetWidget xml castToButton "stopButton"
       rBtn   <- xmlGetWidget xml castToButton "verboseButton"
       jtsBtn <- xmlGetWidget xml castToButton "jumpToStart"
       jtmBtn <- xmlGetWidget xml castToButton "jumpToMiddle"
       jteBtn <- xmlGetWidget xml castToButton "jumpToEnd"
       qBtn   <- xmlGetWidget xml castToMenuItem "quitButton"
       aBtn   <- xmlGetWidget xml castToMenuItem "aboutDialog"
       reBtn  <- xmlGetWidget xml castToMenuItem "refreshButton"
       rsBtn  <- xmlGetWidget xml castToMenuItem "resetButton"
       sView  <- xmlGetWidget xml castToLabel "statusView"
       pBar   <- xmlGetWidget xml castToProgressBar "showProgress"
       vBtn   <- xmlGetWidget xml castToScale "volumeScale"

       return  GUI { 
               mainWin           = mw
               , playBtn         = pBtn
               , pauseBtn        = paBtn
               , stopBtn         = sBtn
               , verboseBtn      = rBtn
               , jumpToStartBtn  = jtsBtn
               , jumpToMiddleBtn = jtmBtn
               , jumpToEndBtn    = jteBtn
               , quitBtn         = qBtn
               , aboutBtn        = aBtn
               , refreshBtn      = reBtn
               , resetBtn        = rsBtn
               , statusView      = sView
               , progBar         = pBar 
               , volumeButton    = vBtn
           }


connectGui :: App -> IO () 
connectGui a = do 
    appState <- newAppState a

    hosts <- getAllHosts appState
    gui   <- getGui appState

    onDestroy (mainWin gui) mainQuit

    on (quitBtn gui) menuItemActivate mainQuit

    -- re-read the config file
    on (refreshBtn gui) menuItemActivate $ do
        stop appState 
        curr <- getApp appState
        newCfg <- readConfig
        setApp curr { appCfg=newCfg } appState 

    -- tell all players to restart 
    on (resetBtn gui) menuItemActivate $ do
        stop appState
        restartAll hosts

    -- change the volume on all players
    on (volumeButton gui) valueChanged $ do
        val <- rangeGetValue (volumeButton gui)
        setVolume (truncate val) hosts 

    onClicked (playBtn gui) $ play appState
    onClicked (pauseBtn gui) $ pause appState
    onClicked (stopBtn gui) $ stop appState

    onClicked (verboseBtn gui) $
        void $ forkIO $ do 
            toggleOsd $ osdRef appState
            showTimecode (osdRef appState) hosts

    onClicked (jumpToStartBtn gui) $
        void $ forkIO $ seekTo 0 hosts
            
    onClicked (jumpToMiddleBtn gui) $
        void $ forkIO $ seekTo 50 hosts
            
    onClicked (jumpToEndBtn gui) $ 
        void $ forkIO $ seekTo 99 hosts
            
    return ()


stop :: AppState -> IO ()
stop state = do 
    hosts <- getAllHosts state
    thread <- getThreadId state
    gui    <- getGui state

    widgetSetSensitivity (playBtn gui)  True
    widgetSetSensitivity (pauseBtn gui) False
    widgetSetSensitivity (stopBtn gui)  False

    forkIO $ stopAll hosts
    case thread of
        Just tid -> do killThread tid
                       setThreadId Nothing state
                       setCount 0 state
                       setPaused False state
                       resetWidgets state
        Nothing  -> return ()


pause :: AppState -> IO ()
pause state = do
    hosts <- getAllHosts state
    thread <- getThreadId state
    gui    <- getGui state
         
    widgetSetSensitivity (playBtn gui)  True
    widgetSetSensitivity (pauseBtn gui) False
    widgetSetSensitivity (stopBtn gui)  True

    case thread of
        Just tid -> do killThread tid
                       setThreadId Nothing state
                       pauseAll hosts
                       setPaused True state
        Nothing  -> return ()


play :: AppState -> IO ()
play state = do
    gui <- getGui state

    widgetSetSensitivity (playBtn gui)  False
    widgetSetSensitivity (pauseBtn gui) True
    widgetSetSensitivity (stopBtn gui)  True

    tid <- forkIO $ waitingTask state
    setThreadId (Just tid) state

    where 
        waitingTask :: AppState -> IO ()
        waitingTask state = do 
            hosts    <- getAllHosts state
            app      <- getApp state
            count    <- getCount state
            paused   <- getPaused state

            let duration = fromIntegral $ getDuration $ appCfg app

            postGUIAsync $ updateTimecode count app 
            postGUIAsync $ updateProgress count app

            if count == 0
                then do osd <- getOsd state
                        startAll hosts
                        showTimecode (osdRef state) hosts
                else when paused $ do 
                    resumeAll hosts
                    setPaused False state
                        


            threadDelay 1000000

            setCount ((count + 1) `mod` duration) state

            waitingTask state


resetWidgets :: AppState -> IO ()
resetWidgets state = do 
    app <- getApp state
    let bar   = progBar    $ appGui app
    let label = statusView $ appGui app

    labelSetMarkup label "<span font='50' font_weight='heavy'>00:00:00</span>" 
    progressBarSetFraction bar 0


updateProgress :: Int -> App -> IO ()
updateProgress i app = do
    let bar  = progBar $ appGui app
    let duration = fromIntegral $ getDuration $ appCfg app
    let total = (1.0 /  duration) * fromIntegral i

    progressBarSetFraction bar total


updateTimecode :: Int -> App -> IO ()
updateTimecode i app = do
    let label = statusView $ appGui app
    labelSetMarkup label str
    
    where
        hours = i `div` 60 `div` 60
        minutes = i `div` 60
        seconds = i `mod` 60
        str = concat [ 
            "<span font='50' font_weight='heavy'>"
            , printf "%02d:%02d:%02d" hours minutes seconds 
            , "</span>" ]


