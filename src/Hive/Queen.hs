{-# LANGUAGE ScopedTypeVariables #-}

module Hive.Queen
  ( startQueen
  , searchQueen
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
startQueen backend = do
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
      serverLoop state@(QueenState scheduler logger drones) = do
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

searchQueen :: Backend -> Process QueenSearchReply
searchQueen backend =
  searchQueen' =<< liftIO (findPeers backend 1000000)
    where
      searchQueen' :: [NodeId] -> Process QueenSearchReply
      searchQueen' (peer:ps) = do
        whereisRemoteAsync peer "queen"
        WhereIsReply _name remoteWhereIs <- expect
        case remoteWhereIs of
          Just queenPid -> return (Just queenPid)
          Nothing       -> searchQueen' ps
      searchQueen' [] = return Nothing