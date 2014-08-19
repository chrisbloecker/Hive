module Hive.RemoteTable
  ( remoteTable
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process (RemoteTable)

import qualified Hive.Problem.Arithmetic (__remoteTable)

-------------------------------------------------------------------------------

remoteTable :: RemoteTable -> RemoteTable
remoteTable =  Hive.Problem.Arithmetic.__remoteTable