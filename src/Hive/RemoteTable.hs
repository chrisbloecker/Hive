module Hive.RemoteTable
  ( remoteTable
  ) where

import Control.Distributed.Process         (RemoteTable)

import qualified  Hive.Problem.TSP.Warrior (__remoteTable)

-------------------------------------------------------------------------------

remoteTable :: RemoteTable -> RemoteTable
remoteTable = Hive.Problem.TSP.Warrior.__remoteTable