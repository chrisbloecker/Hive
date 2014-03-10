module Hive.Commander
  where

import Data.Text.Lazy (pack)

import Control.Distributed.Process (Process, link, send)

import Hive.Types
import Hive.Messages (StrMsg (..), SSolutionC (..))

import qualified Hive.Problem.TSP.Warrior as TSPW
import qualified Hive.Problem.Data.External.Graph as Graph (parse)

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
      Nothing    -> send clientPid $ SSolutionC . Solution . pack $ ("This is not a valid instance of " ++ show problemType) -- ToDo: This message shouldn't be used here
    SSSP -> send clientPid $ SSolutionC . Solution . pack $ "Not yet implemented."
    APSP -> send clientPid $ SSolutionC . Solution . pack $ "Not yet implemented."