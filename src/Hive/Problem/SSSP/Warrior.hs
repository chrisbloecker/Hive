{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.SSSP.Warrior
  ( run
  , __remoteTable
  ) where

import Control.Distributed.Process (Process, ProcessId, send, getSelfPid, receiveWait, match, matchIf, liftIO)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Control.Monad     (forM_)
import Data.Text.Lazy    (pack)
import Data.Binary       (Binary, put, get)
import Data.Typeable     (Typeable)
import Data.DeriveTH     (derive, makeBinary)
import GHC.Generics      (Generic)

import Hive.Types            (Queen, Warrior, Scheduler, Client, Task (..))
import Hive.Problem.Types    (Solution (..))
import Hive.Messages         (WTaskS (..), StrMsg (..), SSolutionC (..), SSendSolutionW (..))
import Hive.Problem.TSP.Permutation (shuffle)
import qualified Hive.Problem.Data.External.Graph as External (Graph)
import qualified Hive.Problem.Data.Internal.Graph as Internal (Graph, Path, size, mkGraphFromExternalGraph, pathLength, shorterPath, distance)

-------------------------------------------------------------------------------

type Worker    = ProcessId
data Register  = Register Worker                          deriving (Generic, Typeable, Show)
data SetGraph  = SetGraph Internal.Graph                  deriving (Generic, Typeable, Show)
data Run       = Run                                      deriving (Generic, Typeable, Show)
data Candidate = Candidate Worker Integer Internal.Path   deriving (Generic, Typeable, Show)
data Terminate = Terminate                                deriving (Generic, Typeable, Show)

data WorkerS   = WorkerS { warrior :: Warrior
                         , graph   :: Internal.Graph
                         } deriving (Eq, Show)

data WarriorS  = WarriorS { taskCount  :: Integer
                          , solutions  :: [(Integer, Internal.Path)]
                          } deriving (Eq, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''Register)
$(derive makeBinary ''SetGraph)
$(derive makeBinary ''Run)
$(derive makeBinary ''Candidate)
$(derive makeBinary ''Terminate)

-------------------------------------------------------------------------------

worker :: (Warrior, Internal.Graph) -> Process ()
worker (warriorPid, graphIn) = undefined

remotable ['worker]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> External.Graph -> Process ()
run queen scheduler client graph = do
  send queen $ StrMsg "New Warrior up!"
  let graph' = Internal.mkGraphFromExternalGraph graph
  send client $ SSolutionC $ Solution (pack . show $ graph') 0