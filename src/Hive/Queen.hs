{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Hive.Queen
  ( QueenSearchReply
  , runQueen
  ) where

import Control.Distributed.Process
  ( Process
  , ProcessMonitorNotification(..)
  , getSelfPid
  , register
  , spawnLocal
  , say
  , receiveWait
  , match
  , matchUnknown
  , monitor
  , send
  )

import Data.Set as Set
import Data.Map as Map

import Hive.Types
import Hive.Messages
import Hive.Logger
import Hive.Scheduler

-------------------------------------------------------------------------------

type QueenSearchReply = Maybe Queen

data QueenState = QueenState { scheduler :: Scheduler
                             , logger    :: Logger
                             , drones    :: Set Drone
                             , cpuInfos  :: Map Drone CPUInfo
                             }
  deriving (Show)

-------------------------------------------------------------------------------

addDrone :: (Drone, CPUInfo) -> QueenState -> QueenState
addDrone (d, cpuInfo) s@(QueenState {..}) = s { drones = d `Set.insert` drones, cpuInfos = Map.insert d cpuInfo cpuInfos }

removeDrone :: Drone -> QueenState -> QueenState
removeDrone d s@(QueenState {..}) = s { drones = d `Set.delete` drones, cpuInfos = d `Map.delete` cpuInfos }

-------------------------------------------------------------------------------

connectDrone :: Drone -> Scheduler -> Logger -> Process ()
connectDrone drone scheduler logger = send drone (QRegisteredD scheduler logger) >> send scheduler (QNewDroneS drone)

-------------------------------------------------------------------------------

runQueen ::Process ()
runQueen = do
  queen     <- getSelfPid
  register "queen" queen
  logger    <- spawnLocal $ runLogger    queen
  scheduler <- spawnLocal $ runScheduler queen logger
  say $ "Queen     is at " ++ show queen
  say $ "Logger    is at " ++ show logger
  say $ "Scheduler is at " ++ show scheduler
  loop $ QueenState scheduler logger Set.empty Map.empty
    where
      loop :: QueenState -> Process ()
      loop state@(QueenState {..}) =
        receiveWait [ match $ \(StrMsg s) -> do
                        say $ "Received StrMsg: " ++ s
                        loop state

                    , match $ \(DRegisterAtQ drone cpuInfo) -> do
                        say $ "Drone registered at " ++ show drone
                        _mon <- monitor drone
                        connectDrone drone scheduler logger
                        loop . addDrone (drone, cpuInfo) $ state

                    , match $ \(CSolveProblemQ request@(ClientRequest client problem)) -> do
                        say "Solve request received..."
                        send scheduler $ QEnqueRequestS request
                        loop state

                    , match $ \(CGetStatisticsQ client) -> do
                        say "Statistics request received..."
                        send client $ QStatisticsC . Statistics $ Map.elems cpuInfos
                        loop state

                    , match $ \(ProcessMonitorNotification _mon drone _reason) -> do
                        say $ show drone ++ " died..."
                        send scheduler $ QDroneDisappearedS drone
                        loop . removeDrone drone $ state

                    , matchUnknown $ do 
                        say "Unknown message received. Discarding..."
                        loop state
                    ]