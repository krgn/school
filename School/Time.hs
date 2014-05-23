module School.Time where


import School.Config
import Data.UnixTime
import Foreign.C.Types
import GHC.Int


getDelayMS :: UnixTime -> IO Int
getDelayMS ts = do
    curr <- getUnixTime
    let diff = diffUnixTime curr ts
    
    putStrLn "about to parse"
    -- UGLY ALERT!!! need a nicer way to convert between CTime and Int
    return $ newts diff

    where
        secs d = abs $ (read (show $ udtSeconds d) :: Int)
        msecs d = abs $ (read (show $ udtMicroSecnods d ):: Int)
        newts d = read (show (secs d) ++ show (msecs d)) :: Int


mkTimestamp :: Host -> IO String
mkTimestamp host = do
    current <- getUnixTime

    let latency = microSecondsToUnixDiffTime $ getLatency host * 1000
    let scheduled = addUnixDiffTime current latency

    return $ formatTimestamp scheduled


parseTimeStamp :: String -> UnixTime
parseTimeStamp ts = UnixTime {
        utSeconds = read sec :: CTime,
        utMicroSeconds = read msec :: Int32
    }
    where 
        pd   = (/= '.')
        sec  = takeWhile pd ts
        msec = tail $ dropWhile pd ts


formatTimestamp :: UnixTime -> String
formatTimestamp u = show (utSeconds u) ++ "." ++ show (utMicroSeconds u)

