{-# LANGUAGE RankNTypes, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Scheduler
  ( runScheduler
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process.Serializable (Serializable, SerializableDict)
import Control.Distributed.Process
--import Control.Distributed.Process.Platform.ManagedProcess

import Hive.Types
import Hive.Messages                (QEnqueRequestS (..), DWorkRequestS (..), QNewDroneS (..), QDroneDisappearedS (..), DAvailableS (..))
import Hive.Scheduler.State
import qualified Hive.Process as Hive (Process (..))

import Hive.Imports.MkBinary

-------------------------------------------------------------------------------

runScheduler :: Queen -> Process ()
runScheduler queenPid = do
  link queenPid
  loop $ mkEmptyState queenPid
    where
      loop :: SchedulerS -> Process ()
      loop state = receiveWait [ -- 
                                 match $ \(QEnqueRequestS (ClientRequest _ _)) -> do
                                   _self <- getSelfPid
                                   loop state

                               -- when a new drone registers, the queen tells the scheduler
                               , match $ \(QNewDroneS drone) ->
                                   loop . addAvailableDrone drone
                                        $ state

                               -- when a drone disappears, the queen tells the scheduler
                               , match $ \(QDroneDisappearedS drone) -> do
                                   let task = findTaskAllocation drone state
                                   loop . removeTaskAllocation drone
                                        . resetTask task
                                        . removeBusyDrone drone
                                        . removeAvailableDrone drone
                                        $ state

                               , match $ \(DAvailableS drone) ->
                                   loop . addAvailableDrone drone
                                        . removeBusyDrone drone
                                        $ state

                               -- if there are tasks, we can supply drones with work
                               , matchIf (\_ -> not . isEmptyQueue $ state) $ \(DWorkRequestS drone) -> do
                                   let task = nextTask state
                                   --send drone $ SWorkReplyD task
                                   loop . tailQueue
                                        . addTaskAllocation drone task
                                        . addBusyDrone drone
                                        . removeAvailableDrone drone
                                        $ state
                               ]

-------------------------------------------------------------------------------

data GetNode = GetNode ProcessId deriving (Generic, Typeable)
instance Binary GetNode where

data ReceiveNode = ReceiveNode NodeId deriving (Generic, Typeable)
instance Binary ReceiveNode where

data Result a = Result a deriving (Generic, Typeable)
instance (Binary a) => Binary (Result a) where

-------------------------------------------------------------------------------

getNode :: Scheduler -> ProcessId -> Process NodeId
getNode scheduler asker = do
  send scheduler (GetNode asker)
  ReceiveNode nodeId <- expect
  return nodeId


runProcess :: Static (SerializableDict b) -> Scheduler -> Hive.Process a b -> a -> Process b
runProcess dict scheduler (Hive.Simple closureGen) x = do
  node <- getNode scheduler =<< getSelfPid
  call dict node (closureGen x)