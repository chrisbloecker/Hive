{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Master
  ( Ticket
  , runMaster
  , runProcess
  , nodeUp
  , findMaster
  , linkMaster
  , request
  , __remoteTable
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Data.Map (Map)

import Hive.Types
import Hive.Problem
import Hive.NetworkUtils
import Hive.Imports.MkBinary

import qualified Data.Map.Strict as M    (empty, insert, delete, lookup)
import qualified Data.List       as L    (delete)
import qualified Hive.Process    as Hive (Process (..))

-------------------------------------------------------------------------------

newtype Master = Master ProcessId deriving (Eq, Generic, Typeable)
instance Show Master where
  show (Master pid) = show pid
instance Binary Master where

newtype Ticket = Ticket { unTicket :: Int } deriving (Eq, Generic, Typeable)
instance Show Ticket where
  show (Ticket t) = show t
instance Binary Ticket where

data State = State { nodes  :: ![NodeId]
                   , ticket :: !Ticket
                   , active :: !(Map ProcessId (Ticket, MonitorRef))
                   }

-------------------------------------------------------------------------------
-- we will only react to these messages, all others can be thrown away
-------------------------------------------------------------------------------

data GetNode = GetNode ProcessId deriving (Generic, Typeable)
instance Binary GetNode where

data NodeUp = NodeUp NodeId deriving (Generic, Typeable)
instance Binary NodeUp where

data NodeDown = NodeDown NodeId deriving (Generic, Typeable)
instance Binary NodeDown where

data ReceiveNode = ReceiveNode NodeId deriving (Generic, Typeable)
instance Binary ReceiveNode where

data Request = Request ProcessId Problem deriving (Generic, Typeable)
instance Binary Request where

data TicketDone = TicketDone ProcessId Ticket Solution deriving (Generic, Typeable)
instance Binary TicketDone where

-------------------------------------------------------------------------------
-- handle the state
-------------------------------------------------------------------------------

mkTicket :: Int -> Ticket
mkTicket = Ticket

mkEmptyState :: State
mkEmptyState = State [] (mkTicket 0) M.empty

insertNode :: NodeId -> State -> State
insertNode n s@(State {..}) = s { nodes = n:nodes }

removeNode :: NodeId -> State -> State
removeNode n s@(State {..}) = s { nodes = n `L.delete` nodes }

nodeCount :: State -> Int
nodeCount (State {..}) = length nodes

peakNode :: State -> NodeId
peakNode (State {..}) = head nodes

tailNodes :: State -> State
tailNodes s@(State {..}) = s { nodes = tail nodes }

getTicket :: State -> Ticket
getTicket (State {..}) = ticket

nextTicket :: State -> State
nextTicket s@(State {..}) = s { ticket = Ticket (unTicket ticket + 1) }

getMonitor :: ProcessId -> State -> Maybe MonitorRef
getMonitor pid (State {..}) = M.lookup pid active >>= Just . snd

registerJob :: ProcessId -> Ticket -> MonitorRef -> State -> State
registerJob pid t mon s@(State {..}) = s { active = M.insert pid (t, mon) active }

deregisterJob :: ProcessId -> State -> State
deregisterJob pid s@(State {..}) = s { active = M.delete pid active }

-------------------------------------------------------------------------------
-- helper functions to keep messages to scheduler clean
-------------------------------------------------------------------------------

nodeUp :: Master -> NodeId -> Process ()
nodeUp (Master master) node = send master (NodeUp node)

getNode :: Master -> ProcessId -> Process NodeId
getNode (Master master) asker = do
  send master (GetNode asker)
  ReceiveNode nodeId <- expect
  return nodeId

request :: Master -> Problem -> Process Ticket
request (Master master) problem = do
  self <- getSelfPid
  send master (Request self problem)
  expect

-------------------------------------------------------------------------------

problemHandler :: (Master, Ticket, Problem) -> Process ()
problemHandler (Master master, ticket, problem) = do
  say $ "problemHandler started, got: "
  say $ "Master:  " ++ show master
  say $ "Ticket:  " ++ show ticket
  say $ "Problem: " ++ show problem
  let solution = handle problem
  self <- getSelfPid
  send master (TicketDone self ticket solution)
  return ()

remotable ['problemHandler]

-------------------------------------------------------------------------------
-- behaviour of the master
-------------------------------------------------------------------------------

masterName :: String
masterName = "master"

findMaster :: Host -> Port -> Timeout -> Process (Maybe Master)
findMaster host port timeout = do
  mPid <- whereisRemote host port masterName timeout
  return $ mPid >>= Just . Master

linkMaster :: Master -> Process ()
linkMaster (Master master) = link master

runMaster :: Process ()
runMaster = do
  self <- getSelfPid
  register masterName self
  say $ "Master at " ++ show self
  loop $ mkEmptyState
    where
      loop :: State -> Process ()
      loop state = receiveWait [ match $ \(NodeUp node) -> do
                                   say $ "Node up " ++ show node
                                   _ <- monitorNode node
                                   loop . insertNode node
                                        $ state

                               , matchIf (\_ -> nodeCount state > 0) $ \(GetNode asker) -> do
                                   send asker (ReceiveNode . peakNode $ state)
                                   loop . tailNodes
                                        $ state

                               , match $ \(Request client problem) -> do
                                   say $ "Request from " ++ show client
                                   let ticket = getTicket state
                                   self <- getSelfPid
                                   (pid, mon) <- flip spawnMonitor ($(mkClosure 'problemHandler) (Master self, ticket, problem)) =<< getSelfNode
                                   send client ticket
                                   loop . nextTicket
                                        . registerJob pid ticket mon
                                        $ state

                               , match $ \(TicketDone pid ticket solution) -> do
                                   say $ "Ticket " ++ show ticket ++ " done by process " ++ show pid
                                   case getMonitor pid state of
                                     Nothing  -> say $ "Can't find monitor for " ++ show pid
                                     Just mon -> unmonitor mon
                                   say $ "Found solution: " ++ show solution
                                   loop . deregisterJob pid
                                        $ state

                               , match $ \(ProcessMonitorNotification _monitorRef process diedReason) -> do
                                  say $ "Process died: " ++ show process ++ ", reason: " ++ show diedReason
                                  case diedReason of
                                    DiedNormal -> say "Nothing to do..."
                                    _          -> say "Maybe I should do something?"
                                  loop state

                               , match $ \(NodeMonitorNotification _monitorRef node diedReason) -> do
                                   say $ "Node down: " ++ show node ++ ", reason: " ++ show diedReason
                                   loop . removeNode node
                                        $ state
                               ]

-------------------------------------------------------------------------------
-- interpretation of Process structure
-------------------------------------------------------------------------------

runProcess :: Master -> Hive.Process a b -> a -> Process b
runProcess master (Hive.Simple sDict closureGen) x = do
  node <- getNode master =<< getSelfPid
  call sDict node (closureGen x)

runProcess master (Hive.Choice p p1 p2) x =
  runProcess master (if p x then p1 else p2) x

runProcess master (Hive.Sequence p1 p2) x = do
  runProcess master p1 x >>= runProcess master p2

runProcess master (Hive.Parallel p1 p2 g) x = do
  helper (runProcess master p1 x)
  helper (runProcess master p2 x)
  res1 <- expect
  res2 <- expect
  return $ res1 `g` res2
    where
      helper = undefined