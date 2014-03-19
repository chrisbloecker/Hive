{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.SSSP.Warrior
  ( run
  , __remoteTable
  ) where

import Control.Distributed.Process         (Process, ProcessId, send, expect, getSelfPid, receiveWait, match)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Control.Monad     (forM_, liftM)
import Control.Arrow     ((&&&))

import Data.Text.Lazy    (pack)
import Data.Maybe        (isJust)
import Data.IntMap       (IntMap)
import Data.Binary       (Binary, put, get)
import Data.Typeable     (Typeable)
import Data.DeriveTH     (derive, makeBinary)
import GHC.Generics      (Generic)

import Hive.Types            (Queen, Warrior, Scheduler, Client, Task (..), Solution (..))
import Hive.Messages         (StrMsg (..), WGiveMeDronesS (..), SYourDronesW (..), SWorkReplyD (..), SSolutionC (..))

import qualified Data.IntMap as Map  ( empty, unionWith, differenceWith, keys, fromListWith, intersectionWith, mapMaybe
                                     , filterWithKey, singleton, union, (\\), lookup)

import qualified Hive.Problem.Data.External.Graph as External (Graph)
import qualified Hive.Problem.Data.Internal.Graph as Internal (Graph, Node, size, mkGraphFromExternalGraph, partition, neighbours, distance, (<+>))

-------------------------------------------------------------------------------

type Worker      = ProcessId
type PathLengths = IntMap Int
type Updates     = Bool
data Register    = Register Worker                              deriving (Generic, Typeable, Show)
data InitMsg     = InitMsg [Worker] Int Internal.Graph          deriving (Generic, Typeable, Show)
data Terminate   = Terminate                                    deriving (Generic, Typeable, Show)
data Tick        = Tick                                         deriving (Generic, Typeable, Show)
data Tock        = Tock Updates                                 deriving (Generic, Typeable, Show)
data Update      = Update PathLengths                           deriving (Generic, Typeable, Show)

data WorkerS   = WorkerS { warrior   :: Warrior
                         , queen     :: Queen
                         , others    :: [Worker]
                         , indicator :: Int
                         , vertices  :: Internal.Graph
                         , paths     :: PathLengths
                         } deriving (Eq, Show)

data WarriorS  = WarriorS { workers :: [Worker]
                          } deriving (Eq, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''Register)
$(derive makeBinary ''InitMsg)
$(derive makeBinary ''Terminate)
$(derive makeBinary ''Tick)
$(derive makeBinary ''Tock)
$(derive makeBinary ''Update)

-------------------------------------------------------------------------------

sendAll :: (Binary a, Typeable a, Generic a) => [Worker] -> a -> Process ()
sendAll ws msg = forM_ ws $ \w -> send w msg

worker :: (Warrior, Queen) -> Process ()
worker (warriorPid, queenPid) = do
  self <- getSelfPid
  send warriorPid $ Register self
  send queenPid $ StrMsg "This worker is working for the Warrior now!"
  -- init all others but self
  initMsg@(InitMsg droneVector indicator graphPartition) <- expect
  send queenPid $ StrMsg ("My initMsg: " ++ show initMsg)
  sendAll (filter (/= self) droneVector) $ Update Map.empty
  workerLoop $ WorkerS warriorPid queenPid droneVector indicator graphPartition Map.empty
    where
      workerLoop :: WorkerS -> Process ()
      workerLoop state@(WorkerS {..}) =
        receiveWait [ match $ \Tick -> do
                        send queen $ StrMsg "Hey yo! I got a tick!"
                        x@(paths', nodes) <- receiveUpdates queen others paths
                        send queen $ StrMsg ("(paths', nodes) = " ++ show x)
                        sendupdates queen others nodes indicator paths' vertices
                        send warrior $ Tock (not . null $ nodes)
                        workerLoop $ state { paths = paths' }

                    , match $ \Terminate ->
                        return ()
                    ]

      receiveUpdates :: Queen -> [Worker] -> PathLengths -> Process (PathLengths, [Internal.Node])
      receiveUpdates queen ws pls = do
        updateMessages <- mapM (\_ -> do {Update update <- expect; return update}) ws
        let updates = foldr (Map.unionWith min) Map.empty updateMessages
        send queen $ StrMsg ("So my updates are: " ++ show updates)
        let pls'    = Map.unionWith min pls updates
        send queen $ StrMsg ("The OLD path lengths are: " ++ show pls)
        send queen $ StrMsg ("The NEW path lengths are: " ++ show pls')
        let nodes   = Map.differenceWith (\old new -> if old > new then Just new else Nothing) pls pls' `Map.union` (updates Map.\\ pls)
        send queen $ StrMsg ("These nodes changed: " ++ show nodes)
        return (pls', Map.keys nodes)

      sendupdates :: Queen -> [Worker] -> [Internal.Node] -> Int -> PathLengths -> Internal.Graph -> Process ()
      sendupdates queen ws ns indicator ps g = do
        let neighbours = map (id &&& Internal.neighbours g) ns
        send queen $ StrMsg ("These need an update: " ++ show neighbours)
        let d1 = filter (isJust . snd) $ concatMap (\(from, tos) -> map (id &&& \to -> Map.lookup from ps Internal.<+> Internal.distance g from to) tos) neighbours
        send queen $ StrMsg ("d1: " ++ show d1)
        let d2 = Map.fromListWith min d1
        send queen $ StrMsg ("d2: " ++ show d2)
        let d3 = Map.mapMaybe id d2
        send queen $ StrMsg ("d3: " ++ show d3)
        let distances = Map.intersectionWith min ps d3 `Map.union` (d3 Map.\\ ps)
        send queen $ StrMsg ("d4: " ++ show distances)
        forM_ (zip [1..] ws) $ \(n, w) -> do
          let update = Map.filterWithKey (\k _ -> k > (n-1)*indicator && k <= n*indicator) distances
          send queen $ StrMsg ("Sending update: " ++ show update)
          send w $ Update update
          return ()


remotable ['worker]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> External.Graph -> Process ()
run queen scheduler client graph = do
  send queen $ StrMsg "New Warrior up!"
  let graph' = Internal.mkGraphFromExternalGraph graph
  send queen $ StrMsg ("This is the graph: " ++ show graph')
  self <- getSelfPid
  send scheduler $ WGiveMeDronesS self (round . (log :: Double -> Double) . fromIntegral $ Internal.size graph')
  (SYourDronesW drones) <- expect
  send queen $ StrMsg $ "Got " ++ (show. length $ drones) ++ " drones"
  sendAll drones $ SWorkReplyD . Task $ $(mkClosure 'worker) (self :: Warrior, queen) -- ToDo: this message should not be generated here
  ws <- collectWorker (length drones) []
  send queen $ StrMsg $ "Workers collected: " ++ show ws
  initWorkers ws graph'
  loop $ WarriorS ws
    where
      loop :: WarriorS -> Process ()
      loop state@(WarriorS {..}) = do
        send queen $ StrMsg "Tick..."
        sendAll workers Tick
        continue <- waitForWorkers (length workers)
        send queen $ StrMsg "It tocked just enough..."
        send queen $ StrMsg (show continue)
        if or continue then
          loop state
        else do
          -- ToDo: collect restults!
          sendAll workers Terminate
          send queen $ StrMsg "Incredilbe... we're done!?"
          send client $ SSolutionC $ Solution (pack . show $ "Seems we're done") 0

      initWorkers :: [Worker] -> Internal.Graph -> Process ()
      initWorkers ws g = do
        let ps = Internal.size g `div` length ws -- partitionSize
        forM_ (zip [1..] ws) $ \(n, w) -> send w $ InitMsg ws (fromIntegral ps) (Internal.partition g ((n-1)*ps) (n*ps))
        mapM_ (uncurry send) $ zip ws ((Update $ Map.singleton 1 0) : repeat (Update Map.empty))
        return ()

      collectWorker :: Int -> [Worker] -> Process [Worker]
      collectWorker n ws | length ws == n  = return ws
                         | otherwise       = do
                            (Register w) <- expect -- ToDo: timeout?
                            collectWorker n (w:ws)

      --waitForWorkers :: Int -> Process Bool
      --waitForWorkers n = liftM or $ mapM (\_ -> do {Tock b <- expect; return b}) [1..n]

      waitForWorkers :: Int -> Process [Bool]
      waitForWorkers n = mapM (\_ -> do {Tock b <- expect; return b}) [1..n]