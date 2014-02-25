module Hive.Scheduler
  ( startScheduler
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process

import Data.Sequence as Sequence

import Hive.Types    ( Queen
                     , Logger
                     , Problem
                     , Client
                     , ClientRequest(ClientRequest)
                     )
import Hive.Messages ( QEnqueProblemS(QEnqueProblemS)
                     , DWorkRequestS(DWorkRequestS)
                     , DWorkDoneS(DWorkDoneS)
                     , SSolutionC(SSolutionC)
                     , SWorkReplyD(SWorkReplyD)
                     )

-------------------------------------------------------------------------------

data SchedulerState = SchedulerState Queen Logger (Seq (Problem, Client))
  deriving (Show)

-------------------------------------------------------------------------------

startScheduler :: Queen -> Logger -> Process ()
startScheduler queenPid loggerPid = do
  link queenPid
  schedulerLoop $ SchedulerState queenPid loggerPid Sequence.empty
    where
      schedulerLoop :: SchedulerState -> Process ()
      schedulerLoop state@(SchedulerState queen logger queue) =
        receiveWait [ match $ \(QEnqueProblemS (ClientRequest client problem)) -> do
                        say "Enqueueing problem..."
                        schedulerLoop $ SchedulerState queen logger (queue |> (problem, client))
                    
                    , match $ \(DWorkDoneS solution client) -> do
                        say "Solution found, sending to client..."
                        send client $ SSolutionC solution
                        schedulerLoop state
                    
                    , matchIf (\_ -> not . Sequence.null $ queue) $ \(DWorkRequestS drone) -> do
                        say $ "Work request from " ++ show drone
                        send drone $ SWorkReplyD (queue `Sequence.index` 0)
                        schedulerLoop $ SchedulerState queen logger (Sequence.drop 1 queue)
                    ]