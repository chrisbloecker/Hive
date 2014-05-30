{-# LANGUAGE RecordWildCards #-}

module Hive.Scheduler.State
  ( SchedulerS
  , mkEmptyState
  , addAvailableDrone
  , removeAvailableDrone
  , addBusyDrone
  , removeBusyDrone
  , addTaskAllocation
  , removeTaskAllocation
  , findTaskAllocation
  , addTask
  , resetTask
  , isEmptyQueue
  , nextTask
  , tailQueue
  ) where

-------------------------------------------------------------------------------

import Hive.Types (Queen, Drone, Logger, Task)

import Data.Map (Map)

import qualified Data.List as L (delete)
import qualified Data.Map  as M (empty, delete, insert, lookup)

-------------------------------------------------------------------------------

data SchedulerS = SchedulerS { queen           :: Queen
                             , availableDrones :: ![Drone]
                             , busyDrones      :: ![Drone]
                             , taskAllocation  :: !(Map Drone Task)
                             , logger          :: Logger
                             , queue           :: ![Task]
                             }
  deriving (Show)

-------------------------------------------------------------------------------

mkEmptyState :: Queen -> Logger -> SchedulerS
mkEmptyState queen logger = SchedulerS queen [] [] M.empty logger []

addAvailableDrone :: Drone -> SchedulerS -> SchedulerS
addAvailableDrone d s@(SchedulerS {..}) = s { availableDrones = d : availableDrones }

removeAvailableDrone :: Drone -> SchedulerS -> SchedulerS
removeAvailableDrone d s@(SchedulerS {..}) = s { availableDrones = d `L.delete` availableDrones }

addBusyDrone :: Drone -> SchedulerS -> SchedulerS
addBusyDrone d s@(SchedulerS {..}) = s { busyDrones = d : busyDrones }

removeBusyDrone :: Drone -> SchedulerS -> SchedulerS
removeBusyDrone d s@(SchedulerS {..}) = s { busyDrones = d `L.delete` busyDrones }

addTaskAllocation :: Drone -> Task -> SchedulerS -> SchedulerS
addTaskAllocation d t s@(SchedulerS {..}) = s { taskAllocation = M.insert d t taskAllocation }

removeTaskAllocation :: Drone -> SchedulerS -> SchedulerS
removeTaskAllocation d s@(SchedulerS {..}) = s { taskAllocation = d `M.delete` taskAllocation }

findTaskAllocation :: Drone -> SchedulerS -> Maybe Task
findTaskAllocation d (SchedulerS {..}) =  M.lookup d taskAllocation

addTask :: Task -> SchedulerS -> SchedulerS
addTask t s@(SchedulerS {..}) = s { queue = queue ++ [t] }

resetTask :: Maybe Task -> SchedulerS -> SchedulerS
resetTask (Just t) s = addTask t s
resetTask Nothing  s = s

isEmptyQueue :: SchedulerS -> Bool
isEmptyQueue (SchedulerS {..}) = null queue

nextTask :: SchedulerS -> Task
nextTask (SchedulerS {..}) = head queue

tailQueue :: SchedulerS -> SchedulerS
tailQueue s@(SchedulerS {..}) = s { queue = tail queue }