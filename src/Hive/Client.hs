module Hive.Client
  ( solveRequest
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import Control.Concurrent.MVar

import Hive.Data
import Hive.Queen

import Data.Text


solveRequest :: Backend -> ProblemType -> Problem -> MVar Solution -> Int -> Process ()
solveRequest backend problemType problem mvar timeout = do
  self     <- getSelfPid
  queenPid <- searchQueen backend
  case queenPid of
    Just queen -> do
      liftIO . putStrLn $ "Queen found at " ++ show queen
      send queen $ IntMsg 42
      send queen $ ChrMsg 'C'
      send queen $ StrMsg "Just a test..."
      send queen $ TxtMsg (pack "Another test!")
      send queen $ CSolveProblemQ $ ClientRequest self problemType problem
      _ <- receiveTimeout timeout
                          [ match $ \(SSolutionC solution) ->
                              liftIO . putMVar mvar $ solution
                          ]
      _ <- liftIO $ tryPutMVar mvar "Sorry, timeout reached... Did't receive a solution in time..."
      return ()
    Nothing -> error "No Queen found... Terminating..."