{-# LANGUAGE OverloadedStrings #-}

module Hive.Drone
  ( startDrone
  ) where

-------------------------------------------------------------------------------

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

startDrone :: Backend -> Process ()
startDrone backend = do
  dronePid <- getSelfPid
  queenPid <- searchQueen backend
  case queenPid of
    Just queen -> do
      send queen $ DRegisterAtQ dronePid
      QRegisteredD scheduler logger <- expect
      link queen
      say "This drone is running..."
      droneLoop $ DroneState queen scheduler logger
    Nothing -> liftIO . putStrLn $ "No Queen found... Terminating..."
  where
    droneLoop :: DroneState -> Process ()
    droneLoop state@(DroneState queen scheduler _logger) = do
      send queen $ StrMsg "Entering drone loop..."
      dronePid <- getSelfPid
      send scheduler $ DWorkRequestS dronePid
      receiveWait [ match $ \(SWorkReplyD (Task closure)) -> do
                      send queen $ StrMsg "A drone got work..."
                      proc <- unClosure closure
                      send queen $ StrMsg "Closure unpacked..."
                      proc
                      send queen $ StrMsg "Closure executed..."
                      droneLoop state
                  
                  , matchUnknown $ do
                      say "Unknown message received. Discarding..."
                      droneLoop state
                  ]