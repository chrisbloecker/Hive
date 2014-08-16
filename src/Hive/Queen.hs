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
  , link
  )

import Data.Set as Set

import Hive.Types
import Hive.Messages
import Hive.Scheduler

-------------------------------------------------------------------------------

type QueenSearchReply = Maybe Queen

data QueenState = QueenState { scheduler :: Scheduler
                             , drones    :: Set Drone
                             }
  deriving (Show)

-------------------------------------------------------------------------------

addDrone :: Drone -> QueenState -> QueenState
addDrone d s@(QueenState {..}) = s { drones = d `Set.insert` drones}

removeDrone :: Drone -> QueenState -> QueenState
removeDrone d s@(QueenState {..}) = s { drones = d `Set.delete` drones }

-------------------------------------------------------------------------------

connectDrone :: Drone -> Scheduler -> Process ()
connectDrone drone scheduler = send drone (QRegisteredD scheduler) >> send scheduler (QNewDroneS drone)

-------------------------------------------------------------------------------

runQueen ::Process ()
runQueen = do
  queen     <- getSelfPid
  register "queen" queen
  scheduler <- spawnLocal $ runScheduler queen
  link scheduler
  say $ "Queen     is at " ++ show queen
  say $ "Scheduler is at " ++ show scheduler
  loop $ QueenState scheduler Set.empty
    where
      loop :: QueenState -> Process ()
      loop state@(QueenState {..}) =
        receiveWait [ match $ \(StrMsg s) -> do
                        say $ "Received StrMsg: " ++ s
                        loop state

                    , match $ \(DRegisterAtQ drone) -> do
                        say $ "Drone registered at " ++ show drone
                        _mon <- monitor drone
                        connectDrone drone scheduler
                        loop . addDrone drone $ state

                    , match $ \(CSolveProblemQ request@(ClientRequest _ _)) -> do
                        say "Solve request received..."
                        send scheduler $ QEnqueRequestS request
                        loop state

                    , match $ \(ProcessMonitorNotification _mon drone _reason) -> do
                        say $ show drone ++ " died..."
                        send scheduler $ QDroneDisappearedS drone
                        loop . removeDrone drone $ state

                    , matchUnknown $ do 
                        say "Unknown message received. Discarding..."
                        loop state
                    ]