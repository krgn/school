module School.Gui where

import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import Network.Socket(withSocketsDo)
import qualified Data.ConfigFile as ConfigFile

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
import Control.Monad
import Control.Monad.Error
import Control.Concurrent
import Data.Either.Utils

data GUI = GUI {
        mainWin :: Window, 
        playBtn :: Button,
        statusView :: Label
    }

data Host = Host {
        getIP :: String,
        getPort :: Integer, 
        getFileName :: String
    } deriving (Show, Eq)

data Config = Config {
        getDuration :: Integer,
        getHosts :: [Host] 
    } deriving (Show,Eq)

main :: FilePath -> IO ()
main gladepath = do 
    -- read Config
    cfg <- runErrorT $ do
        cp <- join $ liftIO $ ConfigFile.readfile ConfigFile.emptyCP "school.cfg"

        duration <- ConfigFile.get cp "DEFAULT" "duration" 
        let sections = ConfigFile.sections cp 

        return Config {
                getDuration = duration
                -- getHosts = hosts
            }

    let schoolConf = forceEither cfg

    -- GUI 
    initGUI
    gui <- loadGlade gladepath
    connectGui gui
    widgetShowAll (mainWin gui)
    mainGUI
        
loadGlade gladepath = 
    do Just xml <- xmlNew gladepath
       -- the main window
       mw    <- xmlGetWidget xml castToWindow "mainWindow"
       pBtn  <- xmlGetWidget xml castToButton "playButton"
       sView <- xmlGetWidget xml castToLabel "statusView"
       return $ GUI mw pBtn sView

connectGui :: GUI -> IO (ConnectId Button)
connectGui gui = do 
    onDestroy (mainWin gui) mainQuit
    onClicked (playBtn gui) (startThread gui)

startThread :: GUI -> IO ()
startThread gui = do
    -- thid <- forkIO (task 10)
    repeatedTimer (putStrLn "hello") (sDelay 1)
    return ()

    where task n = do
            putStrLn $ "hello " ++ show n
            threadDelay 1000000
            if n > 0
                then task (n - 1)
                else killThread =<< myThreadId
        
