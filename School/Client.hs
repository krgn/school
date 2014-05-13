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
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "4000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1

    -- mplayer stuff
    (hand,o,e,pid) <- runInteractiveProcess "mplayer" ["-idle", "-slave"] Nothing Nothing
    hSetBinaryMode hand False
    hSetBuffering hand LineBuffering

    putStrLn "listening for commands"
    loop sock hand

    -- closing everything down
    sClose sock
    terminateProcess pid
    waitForProcess pid
    return ()

loop sock hand = do 
    (conn, _) <- accept sock
    str <- recv conn 2048 
    
    putStr $ "received: " ++ C.unpack str
    
    -- write command to handler
    hPutStr hand $ C.unpack str
    
    sClose conn
    loop sock hand

