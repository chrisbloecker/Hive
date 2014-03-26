{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.SSSP.Warrior
  ( run
  , __remoteTable
  ) where

import Data.Time      (getCurrentTime, diffUTCTime)

import Control.Distributed.Process         (Process, ProcessId, send, expect, getSelfPid, receiveWait, match, liftIO)
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
import Hive.Messages         (WGiveMeDronesS (..), SYourDronesW (..), SWorkReplyD (..), SSolutionC (..), StrMsg (..))

import Hive.Problem.Data.Graph (Graph, Node, size, partition, neighbours, distance, (<+>))

import qualified Data.IntMap as Map  ( empty, unionWith, differenceWith, keys, fromListWith, intersectionWith, mapMaybe
                                     , filterWithKey, singleton, union, (\\), lookup)

-------------------------------------------------------------------------------

type Worker      = ProcessId
type PathLengths = IntMap Int
type Updates     = Bool
data Register    = Register Worker               deriving (Generic, Typeable, Show)
data InitMsg     = InitMsg [Worker] Int Graph    deriving (Generic, Typeable, Show)
data Terminate   = Terminate                     deriving (Generic, Typeable, Show)
data Tick        = Tick                          deriving (Generic, Typeable, Show)
data Tock        = Tock Updates                  deriving (Generic, Typeable, Show)
data Update      = Update PathLengths            deriving (Generic, Typeable, Show)
data Part        = Part PathLengths              deriving (Generic, Typeable, Show)

data WorkerS   = WorkerS { warrior   :: Warrior
                         , others    :: [Worker]
                         , indicator :: Int
                         , vertices  :: !Graph
                         , paths     :: !PathLengths
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
$(derive makeBinary ''Part)

-------------------------------------------------------------------------------

worker :: (Warrior, Queen) -> Process ()
worker (warriorPid, queen) = do
  self <- getSelfPid
  send warriorPid $ Register self
  (InitMsg droneVector indicator graphPartition) <- expect
  send queen $! StrMsg "Init message received..."
  -- init all others but self
  forM_ (filter (/= self) droneVector) $ \w -> send w $ Update Map.empty
  workerLoop $ WorkerS warriorPid droneVector indicator graphPartition Map.empty
    where
      workerLoop :: WorkerS -> Process ()
      workerLoop state@(WorkerS {..}) =
        receiveWait [ match $ \Tick -> do
                        (paths', nodes) <- receiveUpdates others paths
                        sendupdates others nodes indicator paths' vertices
                        send warrior $ Tock (not . null $ nodes)
                        workerLoop $ state { paths = paths' }

                    , match $ \Terminate -> do
                        send warrior $ Part paths
                        return ()
                    ]

      receiveUpdates :: [Worker] -> PathLengths -> Process (PathLengths, [Node])
      receiveUpdates ws pls = do
        updateMessages <- mapM (\_ -> do {Update update <- expect; return update}) ws
        let updates = foldr (Map.unionWith min) Map.empty updateMessages
        let pls'    = Map.unionWith min pls updates
        let nodes   = Map.differenceWith (\old new -> if old > new then Just new else Nothing) pls pls' `Map.union` (updates Map.\\ pls)
        return (pls', Map.keys nodes)

      sendupdates :: [Worker] -> [Node] -> Int -> PathLengths -> Graph -> Process ()
      sendupdates ws ns indicator ps g = do
        let neighbourNodes = map (id &&& neighbours g) ns
        let d1 = filter (isJust . snd) $ concatMap (\(from, tos) -> map (id &&& \to -> Map.lookup from ps <+> distance g from to) tos) neighbourNodes
        let d2 = Map.fromListWith min d1
        let d3 = Map.mapMaybe id d2
        let distances = Map.intersectionWith min ps d3 `Map.union` (d3 Map.\\ ps)
        forM_ (zip [1..] ws) $ \(n, w) -> do
          let update = Map.filterWithKey (\k _ -> k > (n-1)*indicator && k <= n*indicator) distances
          send w $ Update update
          return ()


remotable ['worker]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> Graph -> Process ()
run queen scheduler client graph = do
  self <- getSelfPid
  send scheduler $ WGiveMeDronesS self (round . (log :: Double -> Double) . fromIntegral $ size graph)
  (SYourDronesW drones) <- expect
  forM_ drones $ \d -> send d $ SWorkReplyD . Task $ $(mkClosure 'worker) (self :: Warrior, queen) -- ToDo: this message should not be generated here
  ws <- collectWorker (length drones) []
  initWorkers ws graph
  loop $ WarriorS ws
    where
      loop :: WarriorS -> Process ()
      loop state@(WarriorS {..}) = do
        sendAll workers Tick
        continue <- waitForWorkers (length workers)
        if continue then
          loop state
        else do
          sendAll workers Terminate
          parts <- mapM (\_ -> do {Part part <- expect; return part}) workers
          send client $ SSolutionC $ Solution (pack . show $ foldr Map.union Map.empty parts) 0

      initWorkers :: [Worker] -> Graph -> Process ()
      initWorkers ws g = do
        let ps = size g `div` length ws -- partitionSize
        forM_ (zip [1..] ws) $ \(n, w) -> do
          send queen $! StrMsg $ "Sending partition to " ++ show w
          start <- liftIO getCurrentTime
          let part = partition g ((n-1)*ps) (n*ps)
          send queen $ StrMsg $ "Partition is " ++ show ((n-1)*ps, n*ps)
          send w $ InitMsg ws (fromIntegral ps) part
          end <- liftIO getCurrentTime
          send queen $! StrMsg $ "Creating the partition and sending it took " ++ show (diffUTCTime end start)
        mapM_ (uncurry send) $ zip ws ((Update $ Map.singleton 1 0) : repeat (Update Map.empty))
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