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
                     , Timeout
                     , unTimeout
                     )
import Hive.Messages ( CSolveProblemQ (..)
                     , SSolutionC (..)
                     , QStatisticsC (..)
                     , CGetStatisticsQ (..)
                     )
import Hive.Queen    (searchQueen)

-------------------------------------------------------------------------------

solveRequest :: Backend -> Problem -> MVar Solution -> Timeout -> Timeout -> Process ()
solveRequest backend problem mvar timeout waitForResult = do
  self     <- getSelfPid
  queenPid <- searchQueen backend timeout
  case queenPid of
    Just queen -> do
      liftIO . putStrLn $ "Queen found at " ++ show queen
      send queen $ CSolveProblemQ $ ClientRequest self problem
      _ <- receiveTimeout (unTimeout waitForResult)
                          [ match $ \(SSolutionC solution) ->
                              liftIO . putMVar mvar $ solution
                          ]
      _ <- liftIO . tryPutMVar mvar $ TimeoutReached
      return ()
    Nothing -> error "No Queen found... Terminating..."

getStatistics :: Backend -> MVar Statistics -> Timeout -> Timeout -> Process ()
getStatistics backend mvar timeout waitForResult = do
  self     <- getSelfPid
  queenPid <- searchQueen backend timeout
  case queenPid of
    Just queen -> do
      send queen $ CGetStatisticsQ self
      _ <- receiveTimeout (unTimeout waitForResult)
                          [ match $ \(QStatisticsC statistics) ->
                              liftIO . putMVar mvar $ statistics
                          ]
      _ <- liftIO . tryPutMVar mvar $ Statistics []
      return ()
    Nothing -> error "No Queen found... Terminating..."