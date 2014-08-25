{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Hive.Master.Messaging
  where

-------------------------------------------------------------------------------

import Hive.Types (Master (..), Ticket, Problem, Solution)
import Hive.Imports.MkBinary
import Control.Distributed.Process

-------------------------------------------------------------------------------

data GetNode = GetNode ProcessId deriving (Generic, Typeable)
instance Binary GetNode where

data ReceiveNode = ReceiveNode NodeId deriving (Generic, Typeable)
instance Binary ReceiveNode where

data ReturnNode = ReturnNode NodeId deriving (Generic, Typeable)
instance Binary ReturnNode where

data NodeUp = NodeUp NodeId Int deriving (Generic, Typeable)
instance Binary NodeUp where

data NodeDown = NodeDown NodeId deriving (Generic, Typeable)
instance Binary NodeDown where

data Request = Request ProcessId Problem deriving (Generic, Typeable)
instance Binary Request where

data TicketDone = TicketDone ProcessId Ticket Solution deriving (Generic, Typeable)
instance Binary TicketDone where

-------------------------------------------------------------------------------

nodeUp :: Master -> NodeId -> Int -> Process ()
nodeUp (Master master) node workerCount = send master (NodeUp node workerCount)

getNode :: Master -> ProcessId -> Process NodeId
getNode (Master master) asker = do
  send master (GetNode asker)
  ReceiveNode nodeId <- expect
  return nodeId

returnNode :: Master -> NodeId -> Process ()
returnNode (Master master) node = send master (ReturnNode node)

request :: Master -> Problem -> Process Ticket
request (Master master) problem = do
  self <- getSelfPid
  send master (Request self problem)
  expect