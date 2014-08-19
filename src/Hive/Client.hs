module Hive.Client
  ( solveRequest
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process

import Hive.Types
import Hive.Master

-------------------------------------------------------------------------------

solveRequest :: Host -> Port -> Problem -> Process Ticket
solveRequest masterHost masterPort problem = do
  mMaster <- findMaster masterHost masterPort (milliseconds 500)
  case mMaster of
    Nothing -> error "No master found... Terminating..."
    Just master -> do
      liftIO . putStrLn $ "Master found at " ++ show master
      request master problem