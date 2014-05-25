{-# LANGUAGE OverloadedStrings #-}

module School.Daemon where

import System.IO
import System.Exit
import System.Process
import Network.Socket hiding (recv)
import Network.Socket.ByteString 
import Control.Concurrent
import Control.Monad (unless)
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

    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1

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

    -- closing everything down
    shutdown sock ShutdownBoth 
    sClose sock

    terminateProcess pid
    waitForProcess pid
    exitSuccess

    where 
        prepareHandle h = do
            hSetBinaryMode h False 
            hSetBuffering h LineBuffering   

        emptyHandle debug o = do
            out <- hGetLine o
            putStrLn $ debug ++ ": " ++ out
            emptyHandle debug o


loop :: Socket -> Handle -> IO ()
loop sock hand = do 
    (conn, _) <- accept sock
    str <- recv conn 4096
    sClose conn

    msg <- parseMessage str
     
    unless (snd msg == "quit") $ do
        forkIO $ withDelay hand msg 
        loop sock hand

    where  
        withDelay :: Handle -> (Int, String) -> IO ()
        withDelay h (delay,cmd) = do
            -- threadDelay delay
            -- write command to handler
            hPutStrLn h cmd
            hFlush h

