module Hive.Client
  ( solveRequest
  , getHistory
  ) where

-------------------------------------------------------------------------------

import Control.Concurrent.MVar

import Control.Distributed.Process

import Hive.Types
import Hive.Master

-------------------------------------------------------------------------------

solveRequest :: Host -> Port -> Problem -> MVar Ticket -> Process ()
solveRequest masterHost masterPort problem mvar = do
  mMaster <- findMaster masterHost masterPort (milliseconds 500)
  case mMaster of
    Nothing -> error "No master found... Terminating..."
    Just master -> do
      liftIO . putStrLn $ "Master found at " ++ show master
      ticket <- request master problem
      liftIO $ putMVar mvar ticket
      return ()

getHistory :: Host -> Port -> Int -> Int -> MVar History -> Process ()
getHistory masterHost masterPort from to mvar = do
  mMaster <- findMaster masterHost masterPort (milliseconds 500)
  case mMaster of
    Nothing -> error "No master found... Terminating..."
    Just master -> do
      liftIO . putStrLn $ "Master found at " ++ show master
      history <- requestHistory master (mkTicket from) (mkTicket to)
      liftIO $ putMVar mvar history
      return ()