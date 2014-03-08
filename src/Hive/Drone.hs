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
import Hive.Messages               ( QRegisteredD(QRegisteredD)
                                   , DRegisterAtQ(DRegisterAtQ)
                                   , SWorkReplyD(SWorkReplyD)
                                   , DWorkRequestS(DWorkRequestS)
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
      droneLoop $ DroneState queen scheduler logger
    Nothing -> liftIO . putStrLn $ "No Queen found... Terminating..."
  where
    droneLoop :: DroneState -> Process ()
    droneLoop state@(DroneState _queen scheduler _logger) = do
      dronePid <- getSelfPid
      send scheduler $ DWorkRequestS dronePid
      receiveWait [ match $ \(SWorkReplyD (Task closure)) -> do
                      say "Got work..."
                      _ <- unClosure closure
                      droneLoop state
                  
                  , matchUnknown $ do
                      say "Unknown message received. Discarding..."
                      droneLoop state
                  ]