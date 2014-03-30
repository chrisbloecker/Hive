{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.ANTSP.Warrior
  ( run
  , __remoteTable
  ) where

import Control.Distributed.Process         (Process, ProcessId, send, expect, getSelfPid, receiveWait, match, spawnLocal)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Control.Monad     (forM_, liftM)
import Control.Arrow     ((&&&), second)

import Data.List         (unfoldr)
import Data.Text.Lazy    (pack)
import Data.Maybe        (isJust)
import Data.IntMap       (IntMap)
import Data.Binary       (Binary, put, get)
import Data.Typeable     (Typeable)
import Data.DeriveTH     (derive, makeBinary)
import GHC.Generics      (Generic)

import Hive.Types            (Queen, Warrior, Scheduler, Client, Task (..), Solution (..))
import Hive.Messages         (WGiveMeDronesS (..), SYourDronesW (..), SWorkReplyD (..), SSolutionC (..), StrMsg (..))

import Hive.Problem.Data.Graph (Graph, Node, size, partitions, neighbours, distance, (<+>), pathLength, nodes)

import qualified Data.IntMap as Map  ( empty, unionsWith, differenceWith, keys, fromListWith
                                     , mapMaybe, partitionWithKey, singleton, unions, union, (\\), lookup)

-------------------------------------------------------------------------------

type Worker      = ProcessId
type Updates     = Bool
type Roundtrip   = [Node]
type Pheromones  = Graph
type Ants        = Int
data Register    = Register Worker               deriving (Generic, Typeable, Show)
data InitMsg     = InitMsg Graph Ants            deriving (Generic, Typeable, Show)
data Terminate   = Terminate                     deriving (Generic, Typeable, Show)
data Tick        = Tick Pheromones               deriving (Generic, Typeable, Show)
data Tock        = Tock [Roundtrip]              deriving (Generic, Typeable, Show)

data WorkerS   = WorkerS { warrior    :: Warrior
                         , graph      :: Graph
                         , ants       :: Ants
                         } deriving (Eq, Show)

data WarriorS  = WarriorS { workers    :: [Worker]
                          , g          :: Graph
                          , pheromones :: Graph
                          , solution   :: Roundtrip
                          } deriving (Eq, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''Register)
$(derive makeBinary ''InitMsg)
$(derive makeBinary ''Terminate)
$(derive makeBinary ''Tick)
$(derive makeBinary ''Tock)

-------------------------------------------------------------------------------

worker :: (Warrior, Queen) -> Process ()
worker (warriorPid, queen) = do
  self <- getSelfPid
  send warriorPid $ Register self
  (InitMsg graph ants) <- expect
  send queen $ StrMsg "Init message received..."
  workerLoop $ WorkerS warriorPid graph ants
    where
      workerLoop :: WorkerS -> Process ()
      workerLoop state@(WorkerS {..}) =
        receiveWait [ match $ \(Tick pheromones) -> do
                        send warrior $ Tock (findSolutions ants graph pheromones)
                        workerLoop state

                    , match $ \Terminate ->
                        return ()
                    ]

      findSolutions :: Ants -> Graph -> Pheromones -> [Roundtrip]
      findSolutions = undefined


remotable ['worker]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> Graph -> Process ()
run queen scheduler client graph = do
  self <- getSelfPid
  send scheduler $ WGiveMeDronesS self (round . (log :: Double -> Double) . fromIntegral $ size graph)
  (SYourDronesW drones) <- expect
  forM_ drones $ \d -> send d $ SWorkReplyD . Task $ $(mkClosure 'worker) (self :: Warrior, queen) -- ToDo: this message should not be generated here
  ws <- collectWorker (length drones) []
  initWorkers ws graph (mkPheromones graph) (nodes graph)
  loop $ WarriorS ws
    where
      loop :: WarriorS -> Process ()
      loop state@(WarriorS {..}) = do
        sendAll workers (Tick pheromones)
        continue <- waitForWorkers (length workers)
        if continue then
          loop state
        else do
          sendAll workers Terminate
          send client $ SSolutionC $ Solution (pack . show $ solution) (pathLength solution)

      initWorkers :: [Worker] -> Graph -> Process ()
      initWorkers ws g = do
        forM_ ws $ \w ->
          spawnLocal $ send w $ InitMsg g (size g)
        return ()

      collectWorker :: Int -> [Worker] -> Process [Worker]
      collectWorker n ws | length ws == n  = return ws
                         | otherwise       = do
                            (Register w) <- expect -- ToDo: timeout?
                            collectWorker n (w:ws)

      waitForWorkers :: Int -> Process Bool
      waitForWorkers n = liftM or $ mapM (\_ -> do {Tock b <- expect; return b}) [1..n]

sendAll :: (Binary a, Typeable a, Generic a) => [Worker] -> a -> Process ()
sendAll ws msg = forM_ ws $ \w -> send w msg