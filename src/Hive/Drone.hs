module Hive.Drone
  where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import Hive.Data
import Hive.Queen

startDrone :: Backend -> Process ()
startDrone backend = do
  dronePid <- getSelfPid
  queenPid <- searchQueen backend
  case queenPid of
    Just queen -> do
      send queen $ DRegisterAtQ dronePid
      receiveWait [ match (\(QRegisteredD scheduler logger) -> do
                      liftIO . putStrLn $ "Queen     at " ++ show queen
                      liftIO . putStrLn $ "Logger    at " ++ show logger
                      liftIO . putStrLn $ "Scheduler at " ++ show scheduler
                      link queen
                      droneLoop $ DroneState queen scheduler logger
                    )
                  ]
    Nothing -> liftIO . putStrLn $ "No Queen found... Terminating..."
  where
    droneLoop :: DroneState -> Process ()
    droneLoop state@(DroneState _queen scheduler _logger) = do
      dronePid <- getSelfPid
      send scheduler $ DWorkRequestS dronePid
      receiveWait [ match (\(SWorkReplyD (problem, client)) -> do
                      say "Got work..."
                      send scheduler $ DWorkFinishedS (solve problem) client
                      droneLoop state
                    )
                  , matchUnknown $ do
                      say "Unknown message received. Discarding..."
                      droneLoop state
                  ]

    solve :: Problem -> Solution
    solve = id