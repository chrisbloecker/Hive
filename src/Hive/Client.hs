module Hive.Client
  ( solveRequest
  , getStatistics
  ) where

-------------------------------------------------------------------------------

import Control.Concurrent.MVar     (MVar, putMVar, tryPutMVar)
import Control.Distributed.Process (Process, getSelfPid, liftIO, send, receiveTimeout, match)

import Hive.Types        (Problem, Statistics (..), Solution (..), ClientRequest (..), Timeout, Host, Port, unTimeout, milliseconds)
import Hive.Messages     (CSolveProblemQ (..), SSolutionC (..), QStatisticsC (..), CGetStatisticsQ (..))
import Hive.NetworkUtils (whereisRemote)

-------------------------------------------------------------------------------

solveRequest :: Host -> Port -> Problem -> MVar Solution -> Timeout -> Process ()
solveRequest queenHost queenPort problem mvar waitForResult = do
  self     <- getSelfPid
  queenPid <- whereisRemote queenHost queenPort "queen" (milliseconds 500)
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

getStatistics :: Host -> Port -> MVar Statistics -> Timeout -> Process ()
getStatistics queenHost queenPort mvar waitForResult = do
  self     <- getSelfPid
  queenPid <- whereisRemote queenHost queenPort "queen" (milliseconds 500)
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