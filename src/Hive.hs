{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Main
  where

import System.Environment (getArgs)
import Control.Distributed.Process

import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node hiding (newLocalNode)

import GHC.Generics (Generic)

import Data.Binary
import Data.Typeable
import Data.DeriveTH

import Data.Set as Set

type QueenSearchReply = Maybe ProcessId

type Queen     = ProcessId
type Drone     = ProcessId
type Client    = ProcessId
type Scheduler = ProcessId

type Problem  = String
type Solution = String

data QueenMsg = Register Drone
              | Solve    Problem Client
  deriving (Generic, Typeable, Show)

data DroneMsg = Registered Scheduler
              | Work       Problem
  deriving (Generic, Typeable, Show)

data SchedulerMsg = WorkRequest Drone
                  | WorkReply   Solution
                  | Enqueue     Problem  Client
  deriving (Generic, Typeable, Show)

data QueenState = QueenState Queen Scheduler (Set Drone)
  deriving (Show)

data DroneState = DroneState Queen Scheduler
  deriving (Show)

$(derive makeBinary ''QueenMsg)
$(derive makeBinary ''DroneMsg)
$(derive makeBinary ''SchedulerMsg)


startScheduler :: Queen -> Process ()
startScheduler queenPid = do
  
  -- build function chain
  -- dispatch to drones -> spawn processes
  say "Solving... dummy..."
  return ()


searchQueen :: Backend -> Process QueenSearchReply
searchQueen backend =
  searchQueen' =<< liftIO (findPeers backend 1000000)
    where
      searchQueen' :: [NodeId] -> Process QueenSearchReply
      searchQueen' (peer:ps) = do
        liftIO . putStrLn $ show peer
        whereisRemoteAsync peer "queen"
        WhereIsReply _name remoteWhereIs <- expect
        case remoteWhereIs of
          Just queenPid -> return (Just queenPid)
          Nothing       -> searchQueen' ps
      searchQueen' [] = return Nothing


startQueen :: Backend -> Process ()
startQueen backend = do
  queen <- getSelfPid
  register "queen" queen
  say "Starting Queen..." 
  scheduler <- spawnLocal $ startScheduler queen
  serverLoop (QueenState queen scheduler empty)
    where
      serverLoop :: QueenState -> Process ()
      serverLoop state@(QueenState queen scheduler drones) = do
        receiveWait [ match (\(Register drone) -> do
                        say $ "Drone registered at " ++ show drone
                        send drone $ Registered scheduler
                        serverLoop $ QueenState queen scheduler (drone `insert` drones)
                      )
                    , match (\(Solve problem client) -> do
                        send scheduler $ Enqueue problem client
                        serverLoop state
                      )
                    , matchUnknown $ do 
                        say "Unknown message received. Discarding..."
                        serverLoop state
                    ]


startDrone :: Backend -> Process ()
startDrone backend = do
  dronePid <- getSelfPid
  queen    <- searchQueen backend
  case queen of
    Just queenPid -> do
      link queenPid
      send queenPid $ Register dronePid
      receiveWait [ match (\(Registered scheduler) -> do
                    liftIO . putStrLn $ "Starting Drone..."
                    liftIO . putStrLn $ "Queen: " ++ show queenPid ++ ", Scheduler: " ++ show scheduler
                    droneLoop $ DroneState queenPid scheduler
                    )
                  ]
    Nothing -> liftIO . putStrLn $ "No Queen found... terminating..."
  where
    droneLoop :: DroneState -> Process ()
    droneLoop (DroneState queen scheduler) = do
      dronePid <- getSelfPid
      send scheduler $ WorkRequest dronePid
      receiveWait [ match (\(Work problem) -> do
                      send scheduler $ WorkReply $ solve problem

                    )
                  ]

    solve :: Problem -> Solution
    solve = id


remotable []


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["queen", host, port] -> do
      context <- initializeBackend host port $ __remoteTable initRemoteTable
      node    <- newLocalNode context

      runProcess node (startQueen context)

    ["drone", host, port] -> do
      context <- initializeBackend host port $ __remoteTable initRemoteTable
      node    <- newLocalNode context

      runProcess node (startDrone context)

    other -> do
      putStrLn $ "Your arguments are invalid: " ++ show other
