{-# LANGUAGE OverloadedStrings #-}

module Hive.Client
  ( solveRequest
  , getStatistics
  ) where

-------------------------------------------------------------------------------

import Data.ByteString.Char8 (pack)

import Network.Transport (EndPointAddress(..))
import Control.Distributed.Process (Process, WhereIsReply(..), getSelfPid, liftIO, send, receiveTimeout, match, whereisRemoteAsync, expect)
import Control.Distributed.Process.Internal.Types (NodeId(..))

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

-------------------------------------------------------------------------------

solveRequest :: String -> Problem -> MVar Solution -> Timeout -> Process ()
solveRequest queenAddr problem mvar waitForResult = do
  self     <- getSelfPid
  whereisRemoteAsync (NodeId . EndPointAddress $ pack queenAddr) "queen"
  (WhereIsReply _ queenPid) <- expect
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

getStatistics :: String -> MVar Statistics -> Timeout -> Process ()
getStatistics queenAddr mvar waitForResult = do
  self     <- getSelfPid
  whereisRemoteAsync (NodeId . EndPointAddress $ pack queenAddr) "queen"
  (WhereIsReply _ queenPid) <- expect
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