module Hive.Scheduler
  ( runScheduler
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process  ( Process, link, receiveWait, match, matchIf
                                    , send, getSelfPid, spawnLocal)

import Hive.Commander               (startCommander)
import Hive.Types                   (Queen, Logger, ClientRequest (..))
import Hive.Messages                ( QEnqueRequestS (..), DWorkRequestS (..), SWorkReplyD (..)
                                    , WTaskS (..), QNewDroneS (..), QDroneDisappearedS (..)
                                    , DAvailableS (..)
                                    )
import Hive.Scheduler.State

-------------------------------------------------------------------------------

runScheduler :: Queen -> Logger -> Process ()
runScheduler queenPid loggerPid = do
  link queenPid
  loop (mkEmptyState queenPid loggerPid)
    where
      loop :: SchedulerS -> Process ()
      loop state = receiveWait [ -- 
                                 match $ \(QEnqueRequestS (ClientRequest client problem)) -> do
                                   self <- getSelfPid
                                   _commanderPid <- spawnLocal $ startCommander self client problem
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

                               -- request from a coordinator to add a new task
                               , match $ \(WTaskS _warrior task) ->
                                   loop . addTask task
                                        $ state

                               , match $ \(DAvailableS drone) ->
                                   loop . addAvailableDrone drone
                                        . removeBusyDrone drone
                                        $ state

                               -- if there are tasks, we can supply drones with work
                               , matchIf (\_ -> not . isEmptyQueue $ state) $ \(DWorkRequestS drone) -> do
                                   let task = nextTask state
                                   send drone $ SWorkReplyD task
                                   loop . tailQueue
                                        . addTaskAllocation drone task
                                        . addBusyDrone drone
                                        . removeAvailableDrone drone
                                        $ state
                               ]