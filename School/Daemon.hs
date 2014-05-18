module School.Daemon where

import System.IO
import System.Process
import Data.Bits
import System.Posix.Files
import Network.Socket hiding (recv)
import Network.Socket.ByteString 
import Control.Concurrent
import qualified Data.ByteString.Char8 as C
import Control.Applicative
import qualified GHC.IO.Handle.FD

-- WARNING: when compiled with -threaded, this program is likely not going
-- to work. As soon as one writes to the stdin of the forked process, it 
-- zombifies and any other command with crash this program.

main = withSocketsDo $ do
    -- network stuff
    addrinfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just "4000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1

    --mplayer stuff
    (hand,o,e,pid) <- runInteractiveCommand "mplayer -fs -idle -slave"

    mapM_ (\h -> do hSetBinaryMode h False; hSetBuffering h LineBuffering) [hand, o, e]
    
    forkIO $ emptyHandle "stdout" o
    forkIO $ emptyHandle "stderr" e

    putStrLn "listening for commands"
    loop sock hand
    putStrLn "done.. commands"

    -- closing everything down
    sClose sock
    -- terminateProcess pid
    -- waitForProcess pid
    return ()

    where 
        emptyHandle debug o = do
            out <- hGetLine o
            putStrLn $ debug ++ ": " ++ out
            emptyHandle debug o

            

loop sock hand = do 
    (conn, _) <- accept sock
    str <- recv conn 2048 
    
    putStr $ "received: " ++ C.unpack str
    
    -- write command to handler
    hPutStr hand $ C.unpack str
    hFlush hand
    
    sClose conn
    loop sock hand
  

-- openFIFO path = GHC.IO.Handle.FD.openFile path WriteMode

