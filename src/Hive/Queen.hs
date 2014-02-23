{-# LANGUAGE ScopedTypeVariables #-}

module Hive.Queen
  ( startQueen
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import Data.Set as Set

import Hive.Data
import Hive.Logger
import Hive.Scheduler


data QueenState = QueenState Scheduler Logger (Set Drone)
  deriving (Show)


startQueen :: Backend -> Process ()
startQueen _backend = do
  queen     <- getSelfPid
  register "queen" queen
  logger    <- spawnLocal $ startLogger queen
  scheduler <- spawnLocal $ startScheduler queen logger
  say $ "Queen     is at " ++ show queen
  say $ "Logger    is at " ++ show logger
  say $ "Scheduler is at " ++ show scheduler
  serverLoop $ QueenState scheduler logger Set.empty
    where
      serverLoop :: QueenState -> Process ()
      serverLoop state@(QueenState scheduler logger drones) =
        receiveWait [ match $ \(DRegisterAtQ drone) -> do
                        say $ "Drone registered at " ++ show drone
                        send drone $ QRegisteredD scheduler logger
                        serverLoop $ QueenState scheduler logger (drone `insert` drones)
                    
                    , match $ \(CSolveProblemQ problem) -> do
                        say $ "Solve request: " ++ show problem
                        send scheduler $ QEnqueProblemS problem
                        serverLoop state

                    , matchUnknown $ do 
                        say "Unknown message received. Discarding..."
                        serverLoop state
                    ]