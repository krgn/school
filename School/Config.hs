{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module School.Config 
    (Host(..)
    , SchoolConfig(..)
    , readConfig) where 

import Prelude hiding (lookup)
import Data.Yaml.Config

data Host = Host {
        getIP :: String,
        getPort :: Integer, 
        getFilePath :: String
    } deriving (Show, Eq)

data SchoolConfig = SchoolConfig {
        getDuration :: Integer,
        getHosts :: [Host] 
    } deriving (Show,Eq)

readConfig :: IO SchoolConfig
readConfig = do 
    config <- load "school.yml"
    global <- subconfig "global" config
    servers <- subconfig "servers" config
    duration <- lookup "duration" global
    -- get all hosts from the servers section and turn them into Host values
    serverConfigs <- mapM (\key -> return =<< subconfig key servers) (keys servers)
    hosts <- mapM hostFromConfig serverConfigs
        
    return SchoolConfig { getDuration=duration, getHosts=hosts } 

hostFromConfig :: Config -> IO Host
hostFromConfig cfg = do
    ip <- lookup "ip" cfg
    filepath <- lookup "filepath" cfg
    port <- lookup "port" cfg

    return Host {
        getIP = ip
        , getFilePath = filepath
        , getPort = port
    }
