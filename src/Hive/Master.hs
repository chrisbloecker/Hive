{-# LANGUAGE ScopedTypeVariables, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}
module Hive.Master
  ( Ticket
  , runMaster
  , runProcess
  , nodeUp
  , findMaster
  , linkMaster
  , request
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process

import Data.List (delete)

import Hive.Types
import Hive.NetworkUtils
import Hive.Imports.MkBinary
import qualified Hive.Process as Hive (Process (..), BasicProcess (..))

-------------------------------------------------------------------------------

newtype Master = Master ProcessId deriving (Eq)
instance Show Master where
  show (Master pid) = show pid

newtype Ticket = Ticket Int deriving (Eq)
instance Show Ticket where
  show (Ticket t) = show t

data State = State { nodes  :: ![NodeId]
                   , ticket :: !Ticket
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

-------------------------------------------------------------------------------
-- handle the state
-------------------------------------------------------------------------------

mkTicket :: Int -> Ticket
mkTicket = Ticket

mkEmptyState :: State
mkEmptyState = State [] (mkTicket 0)

insertNode :: NodeId -> State -> State
insertNode n s@(State {..}) = s { nodes = n:nodes }

removeNode :: NodeId -> State -> State
removeNode n s@(State {..}) = s { nodes = n `delete` nodes }

nodeCount :: State -> Int
nodeCount (State {..}) = length nodes

peakNode :: State -> NodeId
peakNode (State {..}) = head nodes

tailNodes :: State -> State
tailNodes s@(State {..}) = s { nodes = tail nodes }

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
request = undefined

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
  say $ "Master at " ++ show self
  loop $ mkEmptyState
    where
      loop :: State -> Process ()
      loop state = receiveWait [ match $ \(NodeUp node) ->
                                   loop . insertNode node
                                        $ state

                               , matchIf (\_ -> nodeCount state > 0) $ \(GetNode asker) -> do
                                   send asker (ReceiveNode . peakNode $ state)
                                   loop . tailNodes
                                        $ state

                               , match $ \(NodeMonitorNotification _monitorRef nodeId _diedReason) -> do
                                   loop . removeNode nodeId
                                        $ state
                               ]

-------------------------------------------------------------------------------
-- interpretation of Process structure
-------------------------------------------------------------------------------

runProcess :: Master -> Hive.Process a b -> a -> Process b
runProcess master (Hive.Simple (Hive.BasicProcess sDict closureGen)) x = do
  node <- getNode master =<< getSelfPid
  call sDict node (closureGen x)

runProcess scheduler (Hive.Sequence p1 p2) x = do
  runProcess scheduler p1 x >>= runProcess scheduler p2

runProcess scheduler (Hive.Parallel p1 p2 g) x = do
  helper (runProcess scheduler p1 x)
  helper (runProcess scheduler p2 x)
  res1 <- expect
  res2 <- expect
  return $ res1 `g` res2
    where
      helper = undefined