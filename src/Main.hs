{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Main
  where

import System.Environment (getArgs)
import Control.Distributed.Process

import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node hiding (newLocalNode)

import Hive.Data
import Hive.Queen
import Hive.Drone


startClient :: Backend -> Problem -> Process ()
startClient backend problem = do
  clientPid <- getSelfPid
  queenPid  <- searchQueen backend
  case queenPid of
    Just queen -> do
      liftIO . putStrLn $ "Queen found at " ++ show queen
      send queen $ CSolveProblemQ $ ClientRequest clientPid TSP problem
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
