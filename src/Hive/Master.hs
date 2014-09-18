{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Master
  ( runMaster
  , nodeUp
  , findMaster
  , linkMaster
  , request
  , requestHistory
  , requestLatestTicket
  , getNode
  , __remoteTable
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process hiding  (closure)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Control.Applicative ((<$>))

import Data.Acid          (openLocalState)
import Data.Acid.Advanced (query', update')

import Data.Time.Clock

import Hive.Types
import Hive.Problem (handle)
import Hive.NetworkUtils

-- we will only react to messages defined in Hive.Master.Messaging
-- other messages will be thrown away
import Hive.Master.Messaging
import Hive.Master.State
import Hive.Master.Persistent

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
  self       <- getSelfPid
  db         <- liftIO $ openLocalState initDatabase
  lastTicket <- query' db GetTicketSeq
  register masterName self
  say $ "Master at " ++ show self
  say $ "Last ticket was " ++ show lastTicket
  loop (nextTicket $ initState lastTicket db)
    where
      loop :: State -> Process ()
      loop state@(State {..}) =
        receiveWait [ match $ \(NodeUp node workerCount) -> do
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
                         now <- liftIO getCurrentTime
                         update' db (UpdateTicketSeq ticket)
                         update' db (InsertEntry (mkEntry ticket problem Nothing (mkTime now) Nothing))
                         loop . nextTicket
                              . registerJob pid ticket mon
                              $ state

                     , match $ \(TicketDone pid ticket solution) -> do
                         say $ "Ticket " ++ show ticket ++ " done by process " ++ show pid
                         say $ "Found solution: " ++ show solution
                         case getMonitor pid state of
                           Nothing  -> say $ "Can't find monitor for " ++ show pid
                           Just mon -> do
                             unmonitor mon
                             mEntry <- query' db (GetEntry ticket)
                             case mEntry of
                               Nothing    -> say "Unknown ticket"
                               Just entry -> do
                                 now <- liftIO getCurrentTime
                                 update' db (UpdateEntry (mkEntry ticket (problem entry) (Just solution) (startTime entry) (Just . mkTime $ now)))
                         loop . deregisterJob pid
                              $ state

                     , match $ \(RequestHistory pid ticketFrom ticketTo) -> do
                         history <- query' db (GetHistory ticketFrom ticketTo)
                         replyHistory pid history
                         loop state

                     , match $ \(RequestLatestTicket pid) -> do
                         replyLatestTicket pid (mkTicket . flip (-) 1 . unTicket $ ticket)
                         loop state

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
