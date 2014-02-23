module Hive.Client
  ( solveRequest
  , searchQueen
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import Control.Concurrent.MVar

import Hive.Data

solveRequest :: Backend -> ProblemType -> Problem -> MVar Solution -> Process ()
solveRequest backend problemType problem mvar = do
  self     <- getSelfPid
  queenPid <- searchQueen backend
  case queenPid of
    Just queen -> do
      send queen $ CSolveProblemQ (ClientRequest self problemType problem)
      SSolutionC solution <- expect
      liftIO $ putMVar mvar solution
    Nothing -> error "No Queen found... Terminating..."

searchQueen :: Backend -> Process QueenSearchReply
searchQueen backend =
  searchQueen' =<< liftIO (findPeers backend 1000000)
    where
      searchQueen' :: [NodeId] -> Process QueenSearchReply
      searchQueen' (peer:ps) = do
        whereisRemoteAsync peer "queen"
        WhereIsReply _name remoteWhereIs <- expect
        case remoteWhereIs of
          Just queenPid -> return (Just queenPid)
          Nothing       -> searchQueen' ps
      searchQueen' [] = return Nothing