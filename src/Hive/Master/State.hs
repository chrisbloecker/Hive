{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Master.State
  where

-------------------------------------------------------------------------------

import Control.Distributed.Process (NodeId, ProcessId, MonitorRef)

import Data.Map (Map)

import Hive.Types

import qualified Data.Map as M (empty, lookup, insert, delete)

-------------------------------------------------------------------------------

data State = State { nodes  :: ![NodeId]
                   , ticket :: !Ticket
                   , active :: !(Map ProcessId (Ticket, MonitorRef))
                   }

-------------------------------------------------------------------------------

mkEmptyState :: State
mkEmptyState = State [] (mkTicket 0) M.empty

insertNode :: NodeId -> State -> State
insertNode n s@(State {..}) = s { nodes = n:nodes }

removeNode :: NodeId -> State -> State
removeNode n s@(State {..}) = s { nodes = filter (/= n) nodes }

nodeCount :: State -> Int
nodeCount (State {..}) = length nodes

peakNode :: State -> NodeId
peakNode (State {..}) = head nodes

tailNodes :: State -> State
tailNodes s@(State {..}) = s { nodes = tail nodes }

getTicket :: State -> Ticket
getTicket (State {..}) = ticket

nextTicket :: State -> State
nextTicket s@(State {..}) = s { ticket = mkTicket . (+1) . unTicket $ ticket }

getMonitor :: ProcessId -> State -> Maybe MonitorRef
getMonitor pid (State {..}) = M.lookup pid active >>= Just . snd

registerJob :: ProcessId -> Ticket -> MonitorRef -> State -> State
registerJob pid t mon s@(State {..}) = s { active = M.insert pid (t, mon) active }

deregisterJob :: ProcessId -> State -> State
deregisterJob pid s@(State {..}) = s { active = M.delete pid active }