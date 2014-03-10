{-# LANGUAGE RecordWildCards #-}

module Hive.Scheduler
  ( startScheduler
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process  (Process, link, receiveWait, match, matchIf, say, send, getSelfPid, spawnLocal)

import Hive.Commander
import Hive.Types                   (Queen, Logger, Task, ClientRequest (..))
import Hive.Messages                ( QEnqueProblemS (..)
                                    , DWorkRequestS (..)
                                    , SWorkReplyD (..)
                                    , WTaskS (..)
                                    )

-------------------------------------------------------------------------------

data SchedulerState = SchedulerState { queen  :: Queen
                                     , logger :: Logger
                                     , queue  :: [Task]
                                     }
  deriving (Show)

-------------------------------------------------------------------------------

startScheduler :: Queen -> Logger -> Process ()
startScheduler queenPid loggerPid = do
  link queenPid
  schedulerLoop $ SchedulerState queenPid loggerPid []
    where
      schedulerLoop :: SchedulerState -> Process ()
      schedulerLoop state@(SchedulerState{..}) = do
        say $ "Currently there are " ++ (show . length $ queue) ++ " tasks in queue."
        receiveWait [ match $ \(QEnqueProblemS (ClientRequest client problem)) -> do
                        say "New problem received, starting commander..."
                        self <- getSelfPid
                        _ <- spawnLocal $ startCommander queen self client problem
                        schedulerLoop state

                    , match $ \(WTaskS _warrior task) -> do
                      say "Enqueueing a task"
                      schedulerLoop $ SchedulerState queen logger (queue ++ [task])

                    , matchIf (\_ -> not . null $ queue) $ \(DWorkRequestS drone) -> do
                        say $ "Work request from " ++ show drone
                        send drone $ SWorkReplyD (head queue)
                        schedulerLoop $ SchedulerState queen logger (tail queue)
                    ]