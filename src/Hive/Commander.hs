module Hive.Commander
  where

import Data.Time      (getCurrentTime, diffUTCTime)

import Control.Distributed.Process (Process, link, send, liftIO)

import Hive.Types    (Queen, Scheduler, Client, Problem (Problem), Instance (Instance), ProblemType (..), Solution (..))
import Hive.Messages (StrMsg (..), SSolutionC (..))

import qualified Hive.Problem.TSP.Warrior  as TSPW
import qualified Hive.Problem.SSSP.Warrior as SSSPW

import qualified Hive.Problem.Data.Graph as Graph (parse)

-------------------------------------------------------------------------------

data WarriorS = WarriorS { scheduler :: Scheduler
                         , client    :: Client
                         }

-------------------------------------------------------------------------------

startCommander :: Queen -> Scheduler -> Client -> Problem -> Process ()
startCommander queenPid schedulerPid clientPid (Problem problemType (Instance inst)) = do
  link schedulerPid
  send queenPid $ StrMsg "New Commander up!"
  case problemType of
    TSP  -> case Graph.parse inst of
      Just graph -> TSPW.run queenPid schedulerPid clientPid graph
      Nothing    -> send clientPid $ SSolutionC InvalidInput -- ToDo: This message shouldn't be used here
    SSSP -> case Graph.parse inst of 
      Just graph -> SSSPW.run queenPid schedulerPid clientPid graph
      Nothing    -> send clientPid $ SSolutionC InvalidInput -- ToDo: This message shouldn't be used here
    APSP -> send clientPid $ SSolutionC NotImplemented