{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.TSP.Warrior
  ( run
  , __remoteTable
  ) where

import Control.Distributed.Process (Process, ProcessId, send, getSelfPid, receiveWait, match, liftIO)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Data.Binary   (Binary, put, get)
import Data.Typeable (Typeable)
import Data.DeriveTH (derive, makeBinary)
import GHC.Generics  (Generic)

-- for shuffle
import Control.Monad     (forM, forM_)
import System.Random     (randomRIO)
import Data.Array.IO     (IOArray)
import Data.Array.MArray (readArray, writeArray, newListArray)

import Hive.Types    (Queen, Warrior, Scheduler, Client, Task (..))
import Hive.Messages (WTaskS (..), StrMsg (..))
import qualified Hive.Problem.Data.External.Graph as External (Graph)
import qualified Hive.Problem.Data.Internal.Graph as Internal (Graph, Path, size, mkDirectedGraphFromExternalGraph, pathLength, shorterPath)

-------------------------------------------------------------------------------

type Worker    = ProcessId
data Register  = Register Worker                   deriving (Generic, Typeable, Show)
data SetGraph  = SetGraph Internal.Graph           deriving (Generic, Typeable, Show)
data Run       = Run                               deriving (Generic, Typeable, Show)
data Solution  = Solution Integer Internal.Path    deriving (Generic, Typeable, Show)
data Terminate = Terminate                         deriving (Generic, Typeable, Show)

data WorkerS   = WorkerS { warrior :: Warrior
                         , graph   :: Internal.Graph
                         } deriving (Eq, Show)

data WarriorS  = WarriorS { workers   :: [Worker]
                          , solutions :: [(Integer, Internal.Path)]
                          } deriving (Eq, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''Register)
$(derive makeBinary ''SetGraph)
$(derive makeBinary ''Run)
$(derive makeBinary ''Solution)
$(derive makeBinary ''Terminate)

-------------------------------------------------------------------------------

worker :: (Warrior, Internal.Graph) -> Process ()
worker (warriorPid, graphIn) = do
  self <- getSelfPid
  send warriorPid $ Register self
  workerLoop $ WorkerS warriorPid graphIn
    where
      workerLoop :: WorkerS -> Process ()
      workerLoop state@(WorkerS {..}) =
        receiveWait [ match $ \(SetGraph g) ->
                        workerLoop $ state { graph = g }

                    , match $ \Run -> do
                        let solve = (liftIO . shuffle) [0 .. (Internal.size graph - 1)]
                        solutions <- mapM (const solve) ([1..100] :: [Integer])
                        let solution = foldr (Internal.shorterPath graph) (head solutions) (tail solutions)
                        case Internal.pathLength graph solution of
                          Just distance -> send warrior $ Solution distance solution
                          Nothing       -> return ()
                        workerLoop state

                    , match $ \Terminate ->
                        return ()
                    ]

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
    j  <- randomRIO (i,n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n' = newListArray (1,n')

remotable ['worker]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> External.Graph -> Process ()
run queen scheduler client graph = do
  send queen $ StrMsg "New Warrior up!"
  let graph' = Internal.mkDirectedGraphFromExternalGraph graph
  self <- getSelfPid
  let task = Task ($(mkClosure 'worker) (self :: Warrior))
  forM_ [1..(Internal.size graph' `div` 25 + 1)] $ \_ ->
    send scheduler $ WTaskS self task
  loop $ WarriorS [] []
    where
      loop :: WarriorS -> Process ()
      loop state@(WarriorS {..}) =
        receiveWait [ match $ \(Register pid) -> do
                        send queen $ StrMsg $ "A worker registered: " ++ show pid
                        loop $ state { workers = pid : workers }

                    , match $ \(Solution int path) ->
                        loop $ state { solutions = (int, path) : solutions }
                    ]