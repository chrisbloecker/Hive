module Hive.Logger
  ( runLogger
  ) where

import Control.Distributed.Process (Process, link)

import Hive.Types                  (Queen)

-------------------------------------------------------------------------------

runLogger :: Queen -> Process ()
runLogger queenPid = do
  link queenPid
  return ()