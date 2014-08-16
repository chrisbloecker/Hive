module Hive.RemoteTable
  ( remoteTable
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process  (RemoteTable)

--import qualified Hive.Process.Chain (__remoteTable)

-------------------------------------------------------------------------------

remoteTable :: RemoteTable -> RemoteTable
remoteTable = id -- Hive.Process.Chain.__remoteTable
         -- . otherTables...