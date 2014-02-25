module Hive.Client
  ( solveRequest
  ) where

-------------------------------------------------------------------------------

-- For CloudHaskell
import Control.Distributed.Process.Backend.SimpleLocalnet (Backend)
import Control.Distributed.Process                        ( Process
                                                          , getSelfPid
                                                          , liftIO
                                                          , send
                                                          , receiveTimeout
                                                          , match
                                                          )

-- "Synchronisation" with calling process
import Control.Concurrent.MVar ( MVar
                               , putMVar
                               , tryPutMVar
                               )

import Data.Text     (pack)

import Hive.Types    ( Problem
                     , Solution(Solution)
                     , ClientRequest(ClientRequest)
                     )
import Hive.Messages ( CSolveProblemQ(CSolveProblemQ)
                     , SSolutionC(SSolutionC)
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
      _ <- liftIO . tryPutMVar mvar . Solution . pack $ "Sorry, timeout reached... Did't receive a solution in time..."
      return ()
    Nothing -> error "No Queen found... Terminating..."