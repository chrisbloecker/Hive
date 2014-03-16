{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Hive.Queen
  ( runQueen
  , searchQueen
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import Data.Text (unpack)
import Data.Set as Set
import Data.Map as Map

import Hive.Types
import Hive.Messages
import Hive.Logger
import Hive.Scheduler

-------------------------------------------------------------------------------

data QueenState = QueenState { scheduler :: Scheduler
                             , logger    :: Logger
                             , drones    :: Set Drone
                             , cpuInfos  :: Map Drone CPUInfo
                             }
  deriving (Show)

-------------------------------------------------------------------------------

runQueen :: Backend -> Process ()
runQueen _backend = do
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
        receiveWait [ match $ \(IntMsg n) -> do
                        say $ "Received IntMsg: " ++ show n
                        loop state

                    , match $ \(ChrMsg c) -> do
                        say $ "Received ChrMsg: " ++ [c]
                        loop state

                    , match $ \(StrMsg s) -> do
                        say $ "Received StrMsg: " ++ s
                        loop state

                    , match $ \(TxtMsg t) -> do
                        say $ "Received TxtMsg: " ++ unpack t
                        loop state

                    , match $ \(DRegisterAtQ drone cpuInfo) -> do
                        say $ "Drone registered at " ++ show drone
                        send drone $ QRegisteredD scheduler logger
                        send scheduler $ QNewDroneS drone
                        loop $ QueenState scheduler logger (drone `Set.insert` drones) (Map.insert drone cpuInfo cpuInfos)

                    , match $ \(CSolveProblemQ problem) -> do
                        say "Solve request received..."
                        send scheduler $ QEnqueProblemS problem
                        loop state

                    , match $ \(CGetStatisticsQ client) -> do
                        send client $ QStatisticsC . Statistics $ Map.elems cpuInfos
                        loop state

                    , matchUnknown $ do 
                        say "Unknown message received. Discarding..."
                        loop state
                    ]

searchQueen :: Backend -> Process QueenSearchReply
searchQueen backend =
  searchQueen' =<< liftIO (findPeers backend 250000)
    where
      searchQueen' :: [NodeId] -> Process QueenSearchReply
      searchQueen' (peer:ps) = do
        whereisRemoteAsync peer "queen"
        WhereIsReply _name remoteWhereIs <- expect
        case remoteWhereIs of
          Just queenPid -> return (Just queenPid)
          Nothing       -> searchQueen' ps
      searchQueen' [] = return Nothing