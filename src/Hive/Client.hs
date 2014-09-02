module Hive.Client
  ( solveRequest
  , getHistory
  , getLatestTicket
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

getLatestTicket :: Host -> Port -> MVar Ticket -> Process ()
getLatestTicket masterHost masterPort mvar = do
  mMaster <- findMaster masterHost masterPort (milliseconds 500)
  case mMaster of
    Nothing -> error "No master found... Terminating..."
    Just master -> do
      liftIO . putStrLn $ "Master found at " ++ show master
      latestTicket <- requestLatestTicket master
      liftIO $ putMVar mvar latestTicket
      return ()