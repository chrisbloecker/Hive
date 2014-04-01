module Hive.Commander
  where

import Control.Distributed.Process (Process, link, send)

import Hive.Types    (Queen, Scheduler, Client, Problem (Problem), Instance (Instance), ProblemType (..), Solution (..))
import Hive.Messages (StrMsg (..), SSolutionC (..))

import qualified Hive.Problem.TSP.Warrior  as TSPW
import qualified Hive.Problem.SSSP.Warrior as SSSPW


import qualified Hive.Problem.Data.Graph            as G (parse, size)
import qualified Hive.Problem.Data.External.PosList as P (parse, convertToGraph)

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
    TSP -> case P.parse inst of
      Just graph -> let graph' = P.convertToGraph graph
                    in  TSPW.run queenPid schedulerPid clientPid graph' (G.size graph')
      Nothing    -> send clientPid $ SSolutionC InvalidInput -- ToDo: This message shouldn't be used here
    SSSP -> case G.parse inst of 
      Just graph -> SSSPW.run queenPid schedulerPid clientPid graph
      Nothing    -> send clientPid $ SSolutionC InvalidInput -- ToDo: This message shouldn't be used here
    APSP -> send clientPid $ SSolutionC NotImplemented