{-# LANGUAGE OverloadedStrings #-}

module Hive.Client
  ( solveRequest
  , getStatistics
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process.Backend.SimpleLocalnet (Backend)
import Control.Distributed.Process                        (Process, getSelfPid, liftIO, send, receiveTimeout, match)

import Control.Concurrent.MVar (MVar, putMVar, tryPutMVar)

import Hive.Types    ( Problem
                     , Statistics (..)
                     , Solution (..)
                     , ClientRequest (..)
                     )
import Hive.Messages ( CSolveProblemQ (..)
                     , SSolutionC (..)
                     , QStatisticsC (..)
                     , CGetStatisticsQ (..)
                     )
import Hive.Queen    (searchQueen)

-------------------------------------------------------------------------------

solveRequest :: Backend -> Problem -> MVar Solution -> Int -> Process ()
solveRequest backend problem mvar timeout = do
  self     <- getSelfPid
  queenPid <- searchQueen backend
  case queenPid of
    Just queen -> do
      liftIO . putStrLn $ "Queen found at " ++ show queen
      send queen $ CSolveProblemQ $ ClientRequest self problem
      _ <- receiveTimeout timeout
                          [ match $ \(SSolutionC solution) ->
                              liftIO . putMVar mvar $ solution
                          ]
      _ <- liftIO . tryPutMVar mvar $ TimeoutReached
      return ()
    Nothing -> error "No Queen found... Terminating..."

getStatistics :: Backend -> MVar Statistics -> Int -> Process ()
getStatistics backend mvar timeout = do
  self     <- getSelfPid
  queenPid <- searchQueen backend
  case queenPid of
    Just queen -> do
      send queen $ CGetStatisticsQ self
      _ <- receiveTimeout timeout
                          [ match $ \(QStatisticsC statistics) ->
                              liftIO . putMVar mvar $ statistics
                          ]
      _ <- liftIO . tryPutMVar mvar $ Statistics []
      return ()
    Nothing -> error "No Queen found... Terminating..."