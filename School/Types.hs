module School.Types where

import Graphics.UI.Gtk hiding (disconnect)
import Data.IORef
import Control.Concurrent
import Control.Monad 
import School.Config

data AppState = AppState {
        osdRef      :: IORef Bool,
        pausedRef   :: IORef Bool,
        countPosRef :: IORef Int,
        threadRef   :: IORef (Maybe ThreadId),
        appRef      :: IORef App
    }


data App = App {
        appGui :: GUI,
        appCfg :: SchoolConfig
    }

data GUI = GUI {
        mainWin         :: Window,

        playBtn         :: Button,
        pauseBtn        :: Button,
        stopBtn         :: Button,
        verboseBtn      :: Button,
        jumpToStartBtn  :: Button,
        jumpToMiddleBtn :: Button,
        jumpToEndBtn    :: Button,

        quitBtn         :: MenuItem,
        aboutBtn        :: MenuItem,
        refreshBtn      :: MenuItem,
        resetBtn        :: MenuItem,

        statusView      :: Label,
        progBar         :: ProgressBar,
        volumeButton    :: Scale
    }


newAppState :: App -> IO AppState
newAppState a = do
    thread  <- newIORef Nothing
    osd     <- newIORef False
    paused  <- newIORef False
    app     <- newIORef a
    counter <- newIORef 0

    return AppState {
               osdRef      = osd,
               pausedRef   = paused,
               countPosRef = counter,
               threadRef   = thread,
               appRef      = app
           }

getAllHosts :: AppState -> IO [Host] 
getAllHosts state = liftM (getHosts . appCfg) (readIORef (appRef state))

getGui :: AppState -> IO GUI
getGui state = liftM appGui (readIORef (appRef state))

getOsd :: AppState -> IO Bool
getOsd state = readIORef $ osdRef state

setOsd :: Bool -> AppState -> IO ()
setOsd b state = writeIORef (osdRef state) b

getThreadId :: AppState -> IO (Maybe ThreadId)
getThreadId state = readIORef $ threadRef state

setThreadId :: Maybe ThreadId -> AppState -> IO ()
setThreadId tid state = writeIORef (threadRef state) tid

getCount :: AppState -> IO Int
getCount state = readIORef $ countPosRef state

setCount :: Int -> AppState -> IO ()
setCount val state = writeIORef (countPosRef state) val

getApp :: AppState -> IO App
getApp state = readIORef $ appRef state

setApp :: App -> AppState -> IO ()
setApp app state = writeIORef (appRef state) app

getPaused :: AppState -> IO Bool
getPaused state = readIORef $ pausedRef state

setPaused :: Bool -> AppState -> IO ()
setPaused t state = writeIORef (pausedRef state) t
