{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Hive.Drone
  ( runDrone
  ) where

-------------------------------------------------------------------------------

import Control.Monad  (join, forM_)
import Control.Distributed.Process ( Process, ProcessId, ProcessMonitorNotification(..), spawnLocal, getSelfPid, link, monitor
                                   , send, expect, receiveWait, match, matchUnknown, unClosure, liftIO, say, sendChan)

import Control.Applicative ((<$>))

-------------------------------------------------------------------------------

import Hive.Types                  ( Queen
                                   , Drone
                                   , Scheduler
                                   , Task (..)
                                   , milliseconds
                                   )
import Hive.Messages               ( QRegisteredD (..)
                                   , DWorkRequestS (..)
                                   , DRegisterAtQ (..)
                                   )
import Hive.NetworkUtils (whereisRemote)

-------------------------------------------------------------------------------

data DroneState = DroneState { queen      :: Queen
                             , scheduler  :: Scheduler
                             , workers    :: [ProcessId]
                             }
  deriving (Show)

-------------------------------------------------------------------------------

runDrone :: String -> String -> Int -> Process ()
runDrone queenHost queenPort workerCount = do
  dronePid <- getSelfPid
  queenPid <- whereisRemote queenHost queenPort "queen" (milliseconds 1000)
  case queenPid of
    Just queen -> do
      send queen $ DRegisterAtQ dronePid
      QRegisteredD scheduler <- expect
      link queen
      forM_ [1 .. workerCount] $ \_ -> spawnLocal (worker dronePid) >>= monitor
      loop $ DroneState queen scheduler []
    Nothing -> liftIO . putStrLn $ "No Queen found... Terminating..."
  where
    loop :: DroneState -> Process ()
    loop state@(DroneState {..}) = do
      dronePid <- getSelfPid
      receiveWait [ --match $ \task@(Task _) -> do
                    --  send (head workers) task
                    --  loop $ state { workers = tail workers }

                    match $ \(ProcessMonitorNotification _mon _drone _reason) -> do
                      spawnLocal (worker dronePid) >>= monitor
                      loop state

                  , match $ \(DWorkRequestS workerPid) -> do
                      send scheduler (DWorkRequestS dronePid)
                      loop $ state { workers = workers ++ [workerPid] }

                  , matchUnknown $ do
                      say "Unknown message received. Discarding..."
                      loop state
                  ]

worker :: Drone -> Process ()
worker coord = do
  link coord
  loop coord
    where
      loop :: Drone -> Process ()
      loop drone = do
        send drone =<< DWorkRequestS <$> getSelfPid
        --Task closure <- expect
        --res <- unClosure closure
        loop drone