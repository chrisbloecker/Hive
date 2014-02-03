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
type Requester = ProcessId
type Scheduler = ProcessId

type Problem = String

data QueenMsg = Register { dronePid  :: Drone     }
              | Solve    { problem   :: Problem
                         , requester :: Requester }
  deriving (Generic, Typeable, Show)

data DroneMsg = Registered { schedulerPid :: Scheduler }
  deriving (Generic, Typeable, Show)

data QueenState = QueenState { scheduler :: Scheduler
                             , drones    :: Set Drone
                             }
  deriving (Show)

data DroneState = DroneState { queen      :: Queen
                             , scheduler_ :: Scheduler
                             }
  deriving (Show)

$(derive makeBinary ''QueenMsg)
$(derive makeBinary ''DroneMsg)


mkQueenState :: Scheduler -> Set Drone -> QueenState
mkQueenState scheduler drones = QueenState scheduler drones

mkDroneState :: Queen -> Scheduler -> DroneState
mkDroneState queen scheduler = DroneState queen scheduler

mkRegisterMsg :: Drone -> QueenMsg
mkRegisterMsg pid = Register { dronePid = pid }

mkRegisteredMsg :: Scheduler -> DroneMsg
mkRegisteredMsg pid = Registered { schedulerPid = pid }


startScheduler :: Queen -> Process ()
startScheduler queenPid = do
  -- build function chain
  -- dispatch to drones -> spawn processes
  -- reply to requester
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
  queenPid <- getSelfPid
  register "queen" queenPid
  say "Starting Queen..." 
  schedulerPid <- spawnLocal $ startScheduler queenPid
  serverLoop queenPid (mkQueenState schedulerPid empty)
    where
      serverLoop :: Queen -> QueenState -> Process ()
      serverLoop queenPid state = do
        liftIO . putStrLn $ "Registered drones: " ++ (show . size $ drones state)
        receiveWait [ match (\(Register { dronePid = pid }) -> do
                        say $ "Drone registered at " ++ show pid
                        send pid $ mkRegisteredMsg (scheduler state)
                        serverLoop queenPid (state {drones = pid `insert` drones state})
                      )
                    , matchUnknown $ do 
                        say "Unknown message received. Discarding..."
                        serverLoop queenPid state
                    ]


startDrone :: Backend -> Process ()
startDrone backend = do
  dronePid <- getSelfPid
  queen    <- searchQueen backend
  case queen of
    Just queenPid -> do
      link queenPid
      send queenPid $ mkRegisterMsg dronePid
      receiveWait [ match (\(Registered { schedulerPid = scheduler }) -> do
                    liftIO . putStrLn $ "Starting Drone..."
                    liftIO . putStrLn $ "Queen: " ++ show queenPid ++ ", Scheduler: " ++ show scheduler
                    droneLoop $ mkDroneState queenPid scheduler
                    )
                  ]
    Nothing -> liftIO . putStrLn $ "No Queen found... terminating..."
  where
    droneLoop :: DroneState -> Process ()
    droneLoop state = do
      return ()
      --receiveWait [ match (\(Registered { schedulerPid = scheduler }) -> do
      --              )
      --            ]


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
