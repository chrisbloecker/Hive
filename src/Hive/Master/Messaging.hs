{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Hive.Master.Messaging
  where

-------------------------------------------------------------------------------

import Control.Distributed.Process

import Hive.Types (Master (..), Ticket, Problem, Solution, History)
import Hive.Imports.MkBinary

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

data RequestHistory = RequestHistory ProcessId Ticket Ticket deriving (Generic, Typeable)
instance Binary RequestHistory where

data ReplyHistory = ReplyHistory History deriving (Generic, Typeable)
instance Binary ReplyHistory where

data RequestLatestTicket = RequestLatestTicket ProcessId deriving (Generic, Typeable)
instance Binary RequestLatestTicket where

data ReplyLatestTicket = ReplyLatestTicket Ticket deriving (Generic, Typeable)
instance Binary ReplyLatestTicket where

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

requestHistory :: Master -> Ticket -> Ticket -> Process History
requestHistory (Master master) fromTicket toTicket = do
  self <- getSelfPid
  send master (RequestHistory self fromTicket toTicket)
  ReplyHistory history <- expect
  return history

replyHistory :: ProcessId -> History -> Process ()
replyHistory pid history = send pid (ReplyHistory history)

requestLatestTicket :: Master -> Process Ticket
requestLatestTicket (Master master) = do
  self <- getSelfPid
  send master (RequestLatestTicket self)
  ReplyLatestTicket ticket <- expect
  return ticket

replyLatestTicket :: ProcessId -> Ticket -> Process ()
replyLatestTicket pid ticket = send pid (ReplyLatestTicket ticket)