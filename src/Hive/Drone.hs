module Hive.Drone
  ( startDrone
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import Hive.Types    ( Queen
                     , Scheduler
                     , Logger
                     , Problem(Problem)
                     , Solution(Solution)
                     , Instance(unInstance)
                     )
import Hive.Messages ( QRegisteredD(QRegisteredD)
                     , DRegisterAtQ(DRegisterAtQ)
                     , SWorkReplyD(SWorkReplyD)
                     , DWorkDoneS(DWorkDoneS)
                     , DWorkRequestS(DWorkRequestS)
                     )
import Hive.Queen    (searchQueen)

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
      receiveWait [ match $ \(SWorkReplyD (problem, client)) -> do
                      say "Got work..."
                      send scheduler $ DWorkDoneS (solve problem) client
                      droneLoop state
                  
                  , matchUnknown $ do
                      say "Unknown message received. Discarding..."
                      droneLoop state
                  ]

    solve :: Problem -> Solution
    solve (Problem _type inst) = Solution $ unInstance inst