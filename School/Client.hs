module School.Client where

import System.IO
import System.Process
import Network.Socket hiding (recv)
import Network.Socket.ByteString 
import Control.Concurrent
import qualified Data.ByteString.Char8 as C

-- (i,o,e,pid) <- createProcess (proc "mplayer" ["-idle", "-slave"]) { std_out = CreatePipe, std_in = CreatePipe, cwd = "/home/k/src/nsplay_client/" }
--  (i,o,e,pid) <- runInteractiveProcess "mplayer" ["-idle", "-slave"] (Just "/home/k/src/nsplay_client") Nothing
--   hPutStrLn i "loadfile ./ginger.mp4"
-- hFlush!
-- hSetBinaryMode
main = withSocketsDo $ do
    -- network stuff
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1

    -- mplayer stuff
    (hand,o,e,pid) <- runInteractiveProcess "mplayer" ["-idle", "-slave"] Nothing Nothing
    hSetBinaryMode hand False

    forkIO $ loop sock hand

    sClose sock
    terminateProcess pid
    waitForProcess pid
    return ()

loop sock hand = do 
    (conn, _) <- accept sock
    str <- recv conn 2048 
    
    -- write command to handler
    hPutStrLn hand $ C.unpack str
    hFlush hand
    
    sClose conn
    loop sock hand

