{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.SSSP.Warrior
  ( run
  , __remoteTable
  ) where

import Control.Distributed.Process (Process, ProcessId, send, expect, getSelfPid)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Control.Monad     (forM_)
import Data.Binary       (Binary, put, get)
import Data.Typeable     (Typeable)
import Data.DeriveTH     (derive, makeBinary)
import GHC.Generics      (Generic)

import Hive.Types            (Queen, Warrior, Scheduler, Client, Task (..))
import Hive.Messages         (StrMsg (..), WGiveMeDronesS (..), SYourDronesW (..), SWorkReplyD (..))
import qualified Hive.Problem.Data.External.Graph as External (Graph)
import qualified Hive.Problem.Data.Internal.Graph as Internal (Graph, Path, size, mkGraphFromExternalGraph)

-------------------------------------------------------------------------------

type Worker    = ProcessId
data Register  = Register Worker                          deriving (Generic, Typeable, Show)
data SetGraph  = SetGraph Internal.Graph                  deriving (Generic, Typeable, Show)
data Run       = Run                                      deriving (Generic, Typeable, Show)
data Candidate = Candidate Worker Integer Internal.Path   deriving (Generic, Typeable, Show)
data Terminate = Terminate                                deriving (Generic, Typeable, Show)
data Tick      = Tick                                     deriving (Generic, Typeable, Show)

data WorkerS   = WorkerS { warrior  :: Warrior
                         , vertices :: Internal.Graph
                         } deriving (Eq, Show)

data WarriorS  = WarriorS { workers :: [Worker]
                          } deriving (Eq, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''Register)
$(derive makeBinary ''SetGraph)
$(derive makeBinary ''Run)
$(derive makeBinary ''Candidate)
$(derive makeBinary ''Terminate)
$(derive makeBinary ''Tick)

-------------------------------------------------------------------------------

worker :: Warrior -> Process ()
worker warriorPid = do
  self <- getSelfPid
  send warriorPid $ Register self

remotable ['worker]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> External.Graph -> Process ()
run queen scheduler client graph = do
  send queen $ StrMsg "New Warrior up!"
  let graph' = Internal.mkGraphFromExternalGraph graph
  send scheduler $ WGiveMeDronesS (round . (log :: Double -> Double) . fromIntegral $ Internal.size graph')
  (SYourDronesW drones) <- expect
  self <- getSelfPid
  sendAll drones $ SWorkReplyD . Task $ $(mkClosure 'worker) (self :: Warrior) -- ToDo: this message should not be generated here
  ws <- collectWorker (length drones) []
  loop $ WarriorS ws
    where
      loop :: WarriorS -> Process ()
      loop _state@(WarriorS {..}) = do
        sendAll workers Tick
        return ()

      collectWorker :: Int -> [Worker] -> Process [Worker]
      collectWorker n ws | length ws == n  = return ws
                         | otherwise       = do
                            (Register w) <- expect -- ToDo: timeout?
                            collectWorker n (w:ws)

      sendAll :: (Binary a, Typeable a, Generic a) => [Worker] -> a -> Process ()
      sendAll ws msg = forM_ ws $ \w -> send w msg