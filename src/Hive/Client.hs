module Hive.Client
  ( solveRequest
  ) where

-------------------------------------------------------------------------------

import Control.Concurrent.MVar     (MVar, putMVar, tryPutMVar)
import Control.Distributed.Process (Process, getSelfPid, liftIO, send, receiveTimeout, match)

import Hive.Types        (Problem, Solution (..), ClientRequest (..), Timeout, Host, Port, unTimeout, milliseconds)
import Hive.Messages     (CSolveProblemQ (..), SSolutionC (..))
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