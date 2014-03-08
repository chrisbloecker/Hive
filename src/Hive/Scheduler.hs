{-# LANGUAGE RecordWildCards #-}

module Hive.Scheduler
  ( startScheduler
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process  (Process, link, receiveWait, match, matchIf, say, send, getSelfPid, spawnLocal)

import Data.Sequence                (Seq, (|>))

import Hive.Commander
import Hive.Types                   (Queen, Logger, Task, ClientRequest (..))
import Hive.Messages                ( QEnqueProblemS (..)
                                    , DWorkRequestS (..)
                                    , DWorkDoneS (..)
                                    , SSolutionC (..)
                                    , SWorkReplyD (..)
                                    , WTaskS (..)
                                    )

import qualified Data.Sequence as S (empty, null, drop, index)

-------------------------------------------------------------------------------

data SchedulerState = SchedulerState { queen  :: Queen
                                     , logger :: Logger
                                     , queue  :: Seq Task
                                     }
  deriving (Show)

-------------------------------------------------------------------------------

startScheduler :: Queen -> Logger -> Process ()
startScheduler queenPid loggerPid = do
  link queenPid
  schedulerLoop $ SchedulerState queenPid loggerPid S.empty
    where
      schedulerLoop :: SchedulerState -> Process ()
      schedulerLoop state@(SchedulerState{..}) =
        receiveWait [ match $ \(QEnqueProblemS (ClientRequest client problem)) -> do
                        say "New problem receives, starting commander..."
                        self <- getSelfPid
                        _ <- spawnLocal $ startCommander self client problem
                        schedulerLoop state
                    
                    -- ToDo: this will not happen, remove the handler
                    , match $ \(DWorkDoneS solution client) -> do
                        say "Solution found, sending to client..."
                        send client $ SSolutionC solution
                        schedulerLoop state
                    
                    , match $ \(WTaskS _warrior task) -> do
                      say "Enqueueing a task"
                      schedulerLoop $ SchedulerState queen logger (queue |> task)
                    
                    , matchIf (\_ -> not . S.null $ queue) $ \(DWorkRequestS drone) -> do
                        say $ "Work request from " ++ show drone
                        send drone $ SWorkReplyD (queue `S.index` 0)
                        schedulerLoop $ SchedulerState queen logger (S.drop 1 queue)
                    ]