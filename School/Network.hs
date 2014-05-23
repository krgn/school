module School.Network where

import School.Time
import Control.Monad
import School.Config
import Data.IORef
import Control.Concurrent
import Network.Socket
import System.IO
import System.IO.Error
import Data.String.Utils (strip)
import qualified Data.ByteString.Char8 as C

restartAll :: [Host] -> IO ()
restartAll = mapM_ (sendMessage "quit") 

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
seekTo t = mapM_ (void . seek t)
    where 
        seek p host = do
            let cmd = "seek " ++ show p ++ " 1"
            void $ sendMessage cmd host

showTimecode :: IORef Bool -> [Host] -> IO ()
showTimecode b hosts = do
    osd <- readIORef b
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
        handler _ = void $ putStrLn "error!"

        connAndSend sock addr = do
            connect sock addr
            hock <- socketToHandle sock WriteMode

            -- set to block buffering, as we use flush to get out our message asap
            hSetBuffering hock (BlockBuffering Nothing)
            
            ts <- mkTimestamp host
            
            hPutStrLn hock (ts ++ " " ++ msg)
            hFlush hock

            hClose hock


parseMessage :: C.ByteString -> IO (Int, String) 
parseMessage bs = do 
    let unpacked = C.unpack bs
    let ts = takeWhile (/= ' ') unpacked
    let command = strip $ dropWhile (/= ' ') unpacked

    delay <- getDelayMS $ parseTimeStamp ts
    
    return (delay, command)

