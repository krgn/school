{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module School.Config 
    (Host(..)
    , Latency
    , SchoolConfig(..)
    , readConfig) where 

import Prelude hiding (lookup)
import Data.Yaml.Config

type Latency = Int

data Host = Host {
        getIP :: String,
        getPort :: String, 
        getLatency :: Latency,
        getFilePath :: String
    } deriving (Show, Eq)

data SchoolConfig = SchoolConfig {
        getDuration :: Integer,
        getHosts :: [Host] 
    } deriving (Show,Eq)


readConfig :: IO SchoolConfig
readConfig = do 
    config        <- load "school.yml"
    global        <- subconfig "global" config
    servers       <- subconfig "servers" config
    duration      <- lookup "duration" global
    latency       <- lookup "latency" global
    serverConfigs <- mapM (\key -> return =<< subconfig key servers) (keys servers)
    -- get all hosts from the servers section and turn them into Host values
    hosts         <- mapM (hostFromConfig latency) serverConfigs
        
    return SchoolConfig { 
        getHosts   = hosts
        , getDuration  = duration
        } 


hostFromConfig :: Latency -> Config -> IO Host
hostFromConfig latency cfg = do
    ip       <- lookup "ip" cfg
    filepath <- lookup "filepath" cfg
    port     <- lookup "port" cfg

    return Host {
        getIP         = ip
        , getLatency  = latency
        , getFilePath = filepath
        , getPort     = port
    }
