{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Master
  ( runMaster
  , nodeUp
  , findMaster
  , linkMaster
  , request
  , getNode
  , __remoteTable
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process hiding  (closure)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Hive.Types
import Hive.Problem (handle)
import Hive.NetworkUtils

-- we will only react to messages defined in Hive.Master.Messaging
-- other messages will be thrown away
import Hive.Master.Messaging
import Hive.Master.State

-------------------------------------------------------------------------------

problemHandler :: (Master, Ticket, Problem) -> Process ()
problemHandler (Master master, ticket, problem) = do
  say $ "problemHandler started, got: "
  say $ "Master:  " ++ show master
  say $ "Ticket:  " ++ show ticket
  say $ "Problem: " ++ show problem
  solution <- handle problem (Master master)
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
      loop state = receiveWait [ match $ \(NodeUp node workerCount) -> do
                                   say $ "Node up: " ++ show node
                                   _ <- monitorNode node
                                   let state' = foldr (\_ s -> insertNode node s) state [1..workerCount]
                                   loop state'

                               , match $ \(ReturnNode node) ->
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