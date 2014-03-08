module Hive.Commander
  where

import Control.Distributed.Process (Process, link)

import Hive.Types

import qualified Hive.Problem.TSP.Warrior as TSPW
import qualified Hive.Problem.Data.External.Graph as EG (parse)

-------------------------------------------------------------------------------

data WarriorS = WarriorS { scheduler :: Scheduler
                         , client    :: Client
                         }

-------------------------------------------------------------------------------

startCommander :: Scheduler -> Client -> Problem -> Process ()
startCommander schedulerPid clientPid (Problem problemType inst) = do
  link schedulerPid
  case problemType of
    TSP  -> TSPW.run schedulerPid clientPid . EG.parse . unInstance $ inst
    SSSP -> undefined
    APSP -> undefined