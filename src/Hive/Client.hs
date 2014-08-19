module Hive.Client
  ( solveRequest
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