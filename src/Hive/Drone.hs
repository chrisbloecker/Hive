{-# LANGUAGE OverloadedStrings #-}

module Hive.Drone
  ( runDrone
  ) where

-------------------------------------------------------------------------------

import System.Process  (readProcess)

import Control.Distributed.Process                        (Process, getSelfPid, link, send, expect,receiveWait, match, matchUnknown, unClosure, liftIO, say)
import Control.Distributed.Process.Backend.SimpleLocalnet (Backend)

-------------------------------------------------------------------------------

import Hive.Types                  ( Queen
                                   , Scheduler
                                   , Logger
                                   , Task (..)
                                   )
import Hive.Messages               ( QRegisteredD (..)
                                   , DRegisterAtQ (..)
                                   , SWorkReplyD (..)
                                   , DWorkRequestS (..)
                                   , StrMsg (..)
                                   )
import Hive.Queen                  (searchQueen)

-------------------------------------------------------------------------------

data DroneState = DroneState Queen Scheduler Logger
  deriving (Show)

-------------------------------------------------------------------------------

runDrone :: Backend -> Process ()
runDrone backend = do
  dronePid <- getSelfPid
  queenPid <- searchQueen backend
  case queenPid of
    Just queen -> do
      cpuInfo <- liftIO $ readProcess "grep" ["model name", "/proc/cpuinfo"] ""
      send queen $ DRegisterAtQ dronePid (tail . dropWhile (/= ':') . takeWhile (/= '\n') $ cpuInfo)
      QRegisteredD scheduler logger <- expect
      link queen
      loop $ DroneState queen scheduler logger
    Nothing -> liftIO . putStrLn $ "No Queen found... Terminating..."
  where
    loop :: DroneState -> Process ()
    loop state@(DroneState queen scheduler _logger) = do
      send queen $ StrMsg "Entering drone loop..."
      dronePid <- getSelfPid
      send scheduler $ DWorkRequestS dronePid
      receiveWait [ match $ \(SWorkReplyD (Task closure)) -> do
                      send queen $ StrMsg "A drone got work..."
                      proc <- unClosure closure
                      send queen $ StrMsg "Closure unpacked..."
                      proc
                      send queen $ StrMsg "Closure executed..."
                      loop state
                  
                  , matchUnknown $ do
                      say "Unknown message received. Discarding..."
                      loop state
                  ]