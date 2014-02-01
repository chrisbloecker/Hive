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
type Queen = ProcessId
type Drone = ProcessId

data RegisterMsg = Register { dronePid :: Drone }
  deriving (Generic, Typeable, Show)

data QueenState = QueenState { drones :: Set Drone }

$(derive makeBinary ''RegisterMsg)

remotable []


mkRegisterMsg :: Drone -> RegisterMsg
mkRegisterMsg pid = Register { dronePid = pid }


searchQueen :: Backend -> Process QueenSearchReply
searchQueen backend =
  searchQueen' =<< liftIO (findPeers backend 2000)
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
startQueen backend = do
  queenPid <- getSelfPid
  register "queen" queenPid
  say "Starting Queen..." >> serverLoop queenPid (QueenState { drones = empty})
    where
      serverLoop :: Queen -> QueenState -> Process ()
      serverLoop queenPid state = do
        liftIO . putStrLn $ "Registered drones: " ++ (show . size $ drones state)
        receiveWait [ match (\(Register { dronePid = pid} ) -> do
                        say $ "Drone registered at " ++ show pid
                        serverLoop queenPid (state {drones = pid `insert` drones state}))
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
      liftIO . putStrLn $ "Queen found at " ++ show queenPid
      send queenPid $ mkRegisterMsg dronePid
    Nothing -> liftIO . putStrLn $ "No Queen found..."


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