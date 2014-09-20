{-# LANGUAGE RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Master.State
  ( State (..)
  , initState
  , insertNode
  , removeNode
  , nodeCount
  , peakNode
  , tailNodes
  , getTicket
  , setTicket
  , nextTicket
  , getMonitor
  , registerJob
  , deregisterJob
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process (NodeId, ProcessId, MonitorRef)

import Data.Map  (Map)
import Data.Acid (AcidState)

import Hive.Types hiding (Entry(ticket))
import Hive.Data.Queue        (Queue)
import Hive.Master.Persistent (Database)

import qualified Data.Map        as M (empty, lookup, insert, delete)
import qualified Hive.Data.Queue as Q

-------------------------------------------------------------------------------

data State = State { nodes  :: !(Queue NodeId)
                   , ticket :: !Ticket
                   , active :: !(Map ProcessId (Ticket, MonitorRef))
                   , db     :: !(AcidState Database)
                   }

-------------------------------------------------------------------------------

initState :: Ticket -> (AcidState Database) -> State
initState t db = State Q.mkEmpty t M.empty db

insertNode :: NodeId -> State -> State
insertNode n s@(State {..}) = s { nodes = Q.insert n nodes }

removeNode :: NodeId -> State -> State
removeNode n s@(State {..}) = s { nodes = Q.remove n nodes }

nodeCount :: State -> Int
nodeCount (State {..}) = Q.size nodes

peakNode :: State -> NodeId
peakNode (State {..}) = fst . Q.peak $ nodes

tailNodes :: State -> State
tailNodes s@(State {..}) = s { nodes = snd . Q.peak $ nodes }

getTicket :: State -> Ticket
getTicket (State {..}) = ticket

setTicket :: Ticket -> State -> State
setTicket t s@(State {..}) = s { ticket = t }

nextTicket :: State -> State
nextTicket s@(State {..}) = s { ticket = mkTicket . (+1) . unTicket $ ticket }

getMonitor :: ProcessId -> State -> Maybe MonitorRef
getMonitor pid (State {..}) = M.lookup pid active >>= Just . snd

registerJob :: ProcessId -> Ticket -> MonitorRef -> State -> State
registerJob pid t mon s@(State {..}) = s { active = M.insert pid (t, mon) active }

deregisterJob :: ProcessId -> State -> State
deregisterJob pid s@(State {..}) = s { active = M.delete pid active }