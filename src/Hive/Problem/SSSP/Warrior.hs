{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.SSSP.Warrior
  ( run
  , __remoteTable
  ) where

import Control.Distributed.Process         (Process, ProcessId, send, expect, getSelfPid, receiveWait, match)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Control.Monad     (forM_, liftM)
import Control.Arrow     ((&&&), second)

import Data.List         (unfoldr)
import Data.Text.Lazy    (pack)
import Data.IntMap       (IntMap)
import Data.Binary       (Binary, put, get)
import Data.Typeable     (Typeable)
import Data.DeriveTH     (derive, makeBinary)
import GHC.Generics      (Generic)

import Hive.Types            (Queen, Warrior, Scheduler, Client, Task (..), Solution (..))
import Hive.Messages         (WGiveMeDronesS (..), SYourDronesW (..), SWorkReplyD (..), SSolutionC (..))

import Hive.Problem.Data.Graph (Graph, Node, size, partitions, neighbours, distance')

import qualified Data.IntMap as Map  ( (!), (\\), empty, unionsWith, differenceWith, keys, fromListWith
                                     , partitionWithKey, singleton, unions, union)

-------------------------------------------------------------------------------

type Worker      = ProcessId
type PathLengths = IntMap Int
type Updates     = Bool
data Register    = Register Worker                 deriving (Generic, Typeable, Show)
data InitMsg a   = InitMsg [Worker] Int (Graph a)  deriving (Generic, Typeable, Show)
data Terminate   = Terminate                       deriving (Generic, Typeable, Show)
data Tick        = Tick                            deriving (Generic, Typeable, Show)
data Tock        = Tock Updates                    deriving (Generic, Typeable, Show)
data Update      = Update PathLengths              deriving (Generic, Typeable, Show)
data Part        = Part PathLengths                deriving (Generic, Typeable, Show)

data WorkerS a = WorkerS { warrior   :: Warrior
                         , others    :: [Worker]
                         , indicator :: Int
                         , vertices  :: !(Graph a)
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

unfoldGraph :: PathLengths -> Int -> Int -> [PathLengths]
unfoldGraph ps parts indicator =
  let unF (m, i) = if i < parts then Just . second (id &&& const (i+1)) $! Map.partitionWithKey (\k _ -> k <= (i+1)*indicator) $! m
                                else Nothing
  in  unfoldr unF (ps, 0)

worker :: Warrior -> Process ()
worker warriorPid = do
  self <- getSelfPid
  send warriorPid $ Register self
  (InitMsg droneVector indicator graphPartition) <- expect
  -- init all others but self
  forM_ (filter (/= self) droneVector) $ \w -> send w $ Update Map.empty
  workerLoop $ WorkerS warriorPid droneVector indicator graphPartition Map.empty
    where
      workerLoop :: WorkerS Int -> Process ()
      workerLoop state@(WorkerS {..}) =
        receiveWait [ match $ \Tick -> do
                        (paths', nodes) <- receiveUpdates others paths
                        sendUpdates others nodes indicator paths' vertices
                        send warrior $ Tock (not . null $ nodes)
                        workerLoop $ state { paths = paths' }

                    , match $ \Terminate -> do
                        send warrior $ Part paths
                        return ()
                    ]

      receiveUpdates :: [Worker] -> PathLengths -> Process (PathLengths, [Node])
      receiveUpdates ws pls = do
        updateMessages <- mapM (\_ -> do {Update update <- expect; return update}) ws
        let updates = Map.unionsWith min updateMessages
        let pls'    = Map.unionsWith min [pls, updates]
        let nodes   = Map.differenceWith (\old new -> if old > new then Just new else Nothing) pls pls' `Map.union` (updates Map.\\ pls)
        return (pls', Map.keys nodes)

      sendUpdates :: [Worker] -> [Node] -> Int -> PathLengths -> Graph Int -> Process ()
      sendUpdates ws ns indicator ps g = do
        let d = Map.fromListWith min
              . concatMap (\(from, tos) -> map (id &&& \to -> ps Map.! from + distance' g from to) tos)
              . map (id &&& neighbours g)
              $! ns
        forM_ (zip ws $! unfoldGraph d (length ws) indicator) $ \(w, part) ->
          send w $ Update part


remotable ['worker, 'unfoldGraph]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> Graph Int -> Process ()
run queen scheduler client graph = do
  self <- getSelfPid
  send scheduler $ WGiveMeDronesS self (round . (log :: Double -> Double) . fromIntegral $ size graph)
  (SYourDronesW drones) <- expect
  forM_ drones $ \d -> send d $ SWorkReplyD . Task $ $(mkClosure 'worker) (self :: Warrior) -- ToDo: this message should not be generated here
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
          send client $ SSolutionC $ Solution (pack . show $ Map.unions parts) 0

      initWorkers :: [Worker] -> Graph Int -> Process ()
      initWorkers ws g = do
        let ps = size g `div` length ws -- partitionSize
        forM_ (zip ws $! partitions g (length ws) ps) $ \(w, part) ->
          send w $ InitMsg ws (fromIntegral ps) part
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