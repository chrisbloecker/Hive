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

data ReturnNode = ReturnNode NodeId deriving (Generic, Typeable)
instance Binary ReturnNode where

getNode :: Master -> ProcessId -> Process NodeId
getNode (Master master) asker = do
  send master (GetNode asker)
  ReceiveNode nodeId <- expect
  return nodeId

returnNode :: Master -> NodeId -> Process ()
returnNode (Master master) node = send master (ReturnNode node)