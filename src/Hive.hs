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

-- messages from drones
data DRegisterAtQ   = DRegisterAtQ Drone             deriving (Generic, Typeable, Show)
data DWorkRequestS  = DWorkRequestS Drone            deriving (Generic, Typeable, Show)
data DWorkFinishedS = DWorkFinishedS Solution        deriving (Generic, Typeable, Show)

-- messages from clients
data CSolveProblemQ = CSolveProblemQ Problem Client  deriving (Generic, Typeable, Show)

-- messages from scheduler
data SSolutionC     = SSolutionC Solution            deriving (Generic, Typeable, Show)
data SWorkReplyD    = SWorkReplyD Problem            deriving (Generic, Typeable, Show)

-- messages from queen
data QRegisteredD   = QRegisteredD Scheduler Logger  deriving (Generic, Typeable, Show)
data QWorkD         = QWorkD Problem                 deriving (Generic, Typeable, Show)
data QEnqueProblemS = QEnqueProblemS Problem Client  deriving (Generic, Typeable, Show)


$(derive makeBinary ''DRegisterAtQ)
$(derive makeBinary ''DWorkRequestS)
$(derive makeBinary ''DWorkFinishedS)
$(derive makeBinary ''CSolveProblemQ)
$(derive makeBinary ''SSolutionC)
$(derive makeBinary ''SWorkReplyD)
$(derive makeBinary ''QRegisteredD)
$(derive makeBinary ''QWorkD)
$(derive makeBinary ''QEnqueProblemS)


data QueenState     = QueenState           Scheduler Logger (Set Drone)                          deriving (Show)
data DroneState     = DroneState     Queen Scheduler Logger                                      deriving (Show)
data SchedulerState = SchedulerState Queen           Logger             (Seq (Problem, Client))  deriving (Show)


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
        receiveWait [ match (\(QEnqueProblemS problem client) -> do
                        schedulerLoop $ SchedulerState queen logger (queue |> (problem, client))
                      )
                    , matchIf (\_ -> not . Sequence.null $ queue) (\(DWorkRequestS drone) -> do
                        send drone $  (fst $ queue `Sequence.index` 0)
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
        receiveWait [ match (\(DRegisterAtQ drone) -> do
                        say $ "Drone registered at " ++ show drone
                        send drone $ QRegisteredD scheduler logger
                        serverLoop $ QueenState scheduler logger (drone `insert` drones)
                      )
                    , match (\(CSolveProblemQ problem client) -> do
                        say $ "Solve request from " ++ show client
                        say $ show problem
                        send scheduler $ QEnqueProblemS problem client
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
      receiveWait [ match (\(SWorkReplyD problem) -> do
                      send scheduler $ DWorkFinishedS $ solve problem
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
      send queen $ CSolveProblemQ problem clientPid
      receiveWait [ match (\(SSolutionC solution) -> do
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
