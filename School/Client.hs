module School.Client where

import System.IO
import System.Process
import Network.Socket hiding (recv)
import Network.Socket.ByteString 
import Control.Concurrent
import qualified Data.ByteString.Char8 as C

-- WARNING: when compiled with -threaded, this program is likely not going
-- to work. As soon as one writes to the stdin of the forked process, it 
-- zombifies and any other command with crash this program.

main = withSocketsDo $ do
    -- network stuff
    addrinfos <- getAddrInfo Nothing (Just "localhost") (Just "4000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1

    -- mplayer stuff
    (hand,o,e,pid) <- runInteractiveProcess "mplayer" ["-fs", "-idle", "-slave"] Nothing Nothing
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

