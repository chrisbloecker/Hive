{-# LANGUAGE OverloadedStrings #-}

module Hive.Client
  ( solveRequest
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process.Backend.SimpleLocalnet (Backend)
import Control.Distributed.Process                        (Process, getSelfPid, liftIO, send, receiveTimeout, match)

import Control.Concurrent.MVar (MVar, putMVar, tryPutMVar)

import Hive.Types    (Problem, Solution (TimeoutReached), ClientRequest (ClientRequest))
import Hive.Messages (CSolveProblemQ(CSolveProblemQ), SSolutionC(SSolutionC))
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