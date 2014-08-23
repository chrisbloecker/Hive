{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Hive.Master.Messaging
  where

import Hive.Types (Master (..))
import Hive.Imports.MkBinary
import Control.Distributed.Process

data GetNode = GetNode ProcessId deriving (Generic, Typeable)
instance Binary GetNode where

data ReceiveNode = ReceiveNode NodeId deriving (Generic, Typeable)
instance Binary ReceiveNode where

getNode :: Master -> ProcessId -> Process NodeId
getNode (Master master) asker = do
  send master (GetNode asker)
  ReceiveNode nodeId <- expect
  return nodeId