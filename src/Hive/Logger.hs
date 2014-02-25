module Hive.Logger
  ( startLogger
  ) where

import Control.Distributed.Process

import Hive.Types

startLogger :: Queen -> Process ()
startLogger queenPid = do
  link queenPid
  return ()