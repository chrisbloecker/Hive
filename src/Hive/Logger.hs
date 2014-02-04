module Hive.Logger
  where

import Control.Distributed.Process

import Hive.Data

startLogger :: Queen -> Process ()
startLogger queenPid = do
  link queenPid
  return ()