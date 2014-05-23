{-# LANGUAGE OverloadedStrings #-}

module School.Daemon where

import System.IO
import System.Process
import System.Posix.Files
import Network.Socket hiding (recv)
import Network.Socket.ByteString 
import Control.Concurrent
import qualified Data.ByteString.Char8 as C
import Data.String.Utils
import School.Time
import School.Network

-- WARNING: when compiled with -threaded, this program is likely not going
-- to work. As soon as one writes to the stdin of the forked process, it 
-- zombifies and any other command with crash this program.

main :: IO ()
main = withSocketsDo $ do
    -- network stuff
    addrinfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just "4000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 1

    let cmd = foldr (\ a b -> a ++ " " ++ b) "" l
                where l = [ "DISPLAY=:0",
                        "mplayer", 
                        "-fs", "-idle", "-slave",
                        -- "-framedrop", 
                        -- "-really-quiet",
                        -- "-ao alsa",
                        -- "-vo vdpau",
                        -- "-channels 6",
                        -- "-ontop",
                        "-aspect 16:9" ]

    --mplayer stuff
    (hand,o,e,pid) <- runInteractiveCommand cmd

    mapM_ prepareHandle [hand, o, e]
    
    forkIO $ emptyHandle "stdout" o
    forkIO $ emptyHandle "stderr" e

    putStrLn "listening for commands"
    loop sock hand
    putStrLn "done.. commands"

    -- closing everything down
    sClose sock
    terminateProcess pid
    waitForProcess pid
    return ()

    where 
        prepareHandle h = do
            putStrLn "initializing handle"   
            hSetBinaryMode h False 
            hSetBuffering h LineBuffering   

        emptyHandle debug o = do
            out <- hGetLine o
            putStrLn $ debug ++ ": " ++ out
            emptyHandle debug o


loop sock hand = do 
    (conn, _) <- accept sock
    str <- recv conn 4096
    
    msg <- parseMessage str
     
    withDelay hand msg

    sClose conn
    loop sock hand

    where  
        withDelay :: Handle -> (Int, String) -> IO ()
        withDelay h (delay,cmd) = do
            putStrLn $ "delay command by: " ++ show delay
            threadDelay delay
            putStrLn $ "writing command " ++ cmd
            -- write command to handler
            hPutStrLn h cmd
            hFlush h
            putStrLn "done writing command"

