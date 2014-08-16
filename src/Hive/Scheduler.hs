module Hive.Scheduler
  ( runScheduler
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process  ( Process, link, receiveWait, match, matchIf
                                    , send, getSelfPid)

import Hive.Types                   (Queen, ClientRequest (..))
import Hive.Messages                ( QEnqueRequestS (..), DWorkRequestS (..), SWorkReplyD (..)
                                    , QNewDroneS (..), QDroneDisappearedS (..), DAvailableS (..)
                                    )
import Hive.Scheduler.State
import Hive.Scheduler.Process

-------------------------------------------------------------------------------

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
                                   send drone $ SWorkReplyD task
                                   loop . tailQueue
                                        . addTaskAllocation drone task
                                        . addBusyDrone drone
                                        . removeAvailableDrone drone
                                        $ state
                               ]