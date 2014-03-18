{-# LANGUAGE RecordWildCards #-}

module Hive.Scheduler
  ( runScheduler
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process  ( Process, link, receiveWait, match, matchIf
                                    , say, send, getSelfPid, spawnLocal)

import Data.List                    (delete)
import Data.Map                     (Map)
import qualified Data.Map as M      (empty, insert, lookup, delete)

import Hive.Commander
import Hive.Types                   (Queen, Drone, Logger, Task, ClientRequest (..))
import Hive.Messages                ( QEnqueProblemS (..), DWorkRequestS (..), SWorkReplyD (..)
                                    , WTaskS (..), SSendSolutionW (..), QNewDroneS (..)
                                    , QDroneDisappearedS (..), WGiveMeDronesS (..), SYourDronesW (..))

-------------------------------------------------------------------------------

data SchedulerS = SchedulerS { queen           :: Queen
                             , availableDrones :: [Drone]
                             , busyDrones      :: [Drone]
                             , taskAllocation  :: Map Drone Task
                             , logger          :: Logger
                             , queue           :: [Task]
                             }
  deriving (Show)

-------------------------------------------------------------------------------

addAvailableDrone :: Drone -> SchedulerS -> SchedulerS
addAvailableDrone d s@(SchedulerS {..}) = s { availableDrones = d : availableDrones }

delAvailableDrone :: Drone -> SchedulerS -> SchedulerS
delAvailableDrone d s@(SchedulerS {..}) = s { availableDrones = d `delete` availableDrones }

addBusyDrone :: Drone -> SchedulerS -> SchedulerS
addBusyDrone d s@(SchedulerS {..}) = s { busyDrones = d : busyDrones }

delBusyDrone :: Drone -> SchedulerS -> SchedulerS
delBusyDrone d s@(SchedulerS {..}) = s { busyDrones = d `delete` busyDrones }

addTaskAllocation :: Drone -> Task -> SchedulerS -> SchedulerS
addTaskAllocation d t s@(SchedulerS {..}) = s { taskAllocation = M.insert d t taskAllocation }

delTaskAllocation :: Drone -> SchedulerS -> SchedulerS
delTaskAllocation d s@(SchedulerS {..}) = s { taskAllocation = d `M.delete` taskAllocation }

addTask :: Task -> SchedulerS -> SchedulerS
addTask t s@(SchedulerS {..}) = s { queue = queue ++ [t] }

resetTask :: Maybe Task -> SchedulerS -> SchedulerS
resetTask (Just t) s = addTask t s
resetTask Nothing  s = s

tailQueue :: SchedulerS -> SchedulerS
tailQueue s@(SchedulerS {..}) = s { queue = tail queue }

-------------------------------------------------------------------------------

runScheduler :: Queen -> Logger -> Process ()
runScheduler queenPid loggerPid = do
  link queenPid
  loop $ SchedulerS queenPid [] [] M.empty loggerPid []
    where
      loop :: SchedulerS -> Process ()
      loop state@(SchedulerS{..}) = do
        say $ "Currently there are " ++ (show . length $ queue) ++ " tasks in queue."
        receiveWait [ match $ \(QEnqueProblemS (ClientRequest client problem)) -> do
                        self <- getSelfPid
                        commanderPid <- spawnLocal $ startCommander queen self client problem
                        send commanderPid SSendSolutionW
                        loop state

                    -- when a new drone registers, the queen tells the scheduler
                    , match $ \(QNewDroneS drone) ->
                        loop . addAvailableDrone drone $ state

                    -- when a drone disappears, the quenns tells the scheduler
                    , match $ \(QDroneDisappearedS drone) -> do
                        let task = M.lookup drone taskAllocation
                        loop . delTaskAllocation drone . resetTask task . delBusyDrone drone . delAvailableDrone drone $ state

                    , match $ \(WGiveMeDronesS warrior amount) -> do
                        let drones = take (fromIntegral amount) availableDrones
                        send warrior $ SYourDronesW drones
                        loop . foldr ((.) . (\d -> addBusyDrone d . delAvailableDrone d)) id drones $ state

                    -- request from a coordinator to add a new task
                    , match $ \(WTaskS _warrior task) ->
                        loop . addTask task $ state

                    -- if there are tasks, we can supply drones with work
                    , matchIf (\_ -> not . null $ queue) $ \(DWorkRequestS drone) -> do
                        let task = head queue
                        send drone $ SWorkReplyD task
                        loop . tailQueue . addTaskAllocation drone task . addBusyDrone drone . delAvailableDrone drone $ state
                    ]