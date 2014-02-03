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

import Data.Set      as Set
import Data.Sequence as Sequence

type QueenSearchReply = Maybe ProcessId

type Queen     = ProcessId
type Drone     = ProcessId
type Client    = ProcessId
type Scheduler = ProcessId
type Logger    = ProcessId

type Problem  = String
type Solution = String

data QueenMsg = Register Drone
              | Solve    Problem Client
  deriving (Generic, Typeable, Show)

data DroneMsg = Registered Scheduler Logger
              | Work       Problem
  deriving (Generic, Typeable, Show)

data SchedulerMsg = WorkRequest Drone
                  | WorkReply   Solution
                  | Enqueue     Problem  Client
  deriving (Generic, Typeable, Show)

data ClientMsg = Solved Solution
  deriving (Generic, Typeable, Show)

data QueenState     = QueenState           Scheduler Logger (Set Drone)                          deriving (Show)
data DroneState     = DroneState     Queen Scheduler Logger                                      deriving (Show)
data SchedulerState = SchedulerState Queen           Logger             (Seq (Problem, Client))  deriving (Show)

$(derive makeBinary ''QueenMsg)
$(derive makeBinary ''DroneMsg)
$(derive makeBinary ''SchedulerMsg)
$(derive makeBinary ''ClientMsg)


startLogger :: Queen -> Process ()
startLogger queen = do
  link queen
  return ()


startScheduler :: Queen -> Logger -> Process ()
startScheduler queenPid loggerPid = do
  link queenPid
  schedulerLoop $ SchedulerState queenPid loggerPid Sequence.empty
    where
      schedulerLoop :: SchedulerState -> Process ()
      schedulerLoop state@(SchedulerState queen logger queue) = do
        receiveWait [ match (\(Enqueue problem client) -> do
                        schedulerLoop $ SchedulerState queen logger (queue |> (problem, client))
                      )
                    , matchIf (\_ -> not . Sequence.null $ queue) (\(WorkRequest drone) -> do
                        send drone $ Work (fst $ queue `Sequence.index` 0)
                        schedulerLoop $ SchedulerState queen logger (Sequence.drop 1 queue)
                      )
                    , matchUnknown $ do
                        say "Unknown message received. Discarding..."
                        schedulerLoop state
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


startQueen :: Backend -> Process ()
startQueen _backend = do
  queen     <- getSelfPid
  register "queen" queen
  logger    <- spawnLocal $ startLogger queen
  scheduler <- spawnLocal $ startScheduler queen logger
  say $ "Queen     is at " ++ show queen
  say $ "Logger    is at " ++ show logger
  say $ "Scheduler is at " ++ show scheduler
  serverLoop (QueenState scheduler logger Set.empty)
    where
      serverLoop :: QueenState -> Process ()
      serverLoop state@(QueenState scheduler logger drones) = do
        receiveWait [ match (\(Register drone) -> do
                        say $ "Drone registered at " ++ show drone
                        send drone $ Registered scheduler logger
                        serverLoop $ QueenState scheduler logger (drone `insert` drones)
                      )
                    , match (\(Solve problem client) -> do
                        say $ "Solve request from " ++ show client
                        say $ show problem
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
  queenPid <- searchQueen backend
  case queenPid of
    Just queen -> do
      send queen $ Register dronePid
      receiveWait [ match (\(Registered scheduler logger) -> do
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
      send scheduler $ WorkRequest dronePid
      receiveWait [ match (\(Work problem) -> do
                      send scheduler $ WorkReply $ solve problem
                      droneLoop state
                    )
                  , matchUnknown $ do
                      say "Unknown message received. Discarding..."
                      droneLoop state
                  ]

    solve :: Problem -> Solution
    solve = id


startClient :: Backend -> Problem -> Process ()
startClient backend problem = do
  clientPid <- getSelfPid
  queenPid  <- searchQueen backend
  case queenPid of
    Just queen -> do
      liftIO . putStrLn $ "Queen found at " ++ show queen
      send queen $ Register clientPid
      --send queen $ Solve problem clientPid
      receiveWait [ match (\(Solved solution) -> do
                      liftIO . putStrLn $ "Solution: " ++ show solution
                    )
                  , matchUnknown $ do
                      liftIO . putStrLn $ "Something went wrong..."
                  ]
    Nothing -> liftIO . putStrLn $ "No Queen found... Terminating..."


remotable []


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["queen", host, port] -> do
      context <- initializeBackend host port $ __remoteTable initRemoteTable
      node    <- newLocalNode context

      runProcess node $ startQueen context

    ["drone", host, port] -> do
      context <- initializeBackend host port $ __remoteTable initRemoteTable
      node    <- newLocalNode context

      runProcess node $ startDrone context
    
    ["client", host, port, problem] -> do
      context <- initializeBackend host port $ __remoteTable initRemoteTable
      node    <- newLocalNode context

      runProcess node $ startClient context problem

    other -> do
      putStrLn $ "Your arguments are invalid: " ++ show other
