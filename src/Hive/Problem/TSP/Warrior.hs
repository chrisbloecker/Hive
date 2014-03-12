{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.TSP.Warrior
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
import qualified Hive.Problem.Data.Internal.Graph as Internal (Graph, Path, size, mkGraphFromExternalGraph, pathLength, shorterPath)

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

data WarriorS  = WarriorS { taskCount  :: Int
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
worker (warriorPid, graphIn) = do
  self <- getSelfPid
  send warriorPid $ Register self
  workerLoop $ WorkerS warriorPid graphIn
    where
      workerLoop :: WorkerS -> Process ()
      workerLoop state@(WorkerS {..}) = do
        send warrior $ StrMsg "Entering worker loop..."
        receiveWait [ match $ \(SetGraph g) ->
                        workerLoop $ state { graph = g }

                    , match $ \Run -> do
                        let solve = (liftIO . shuffle) [0 .. (Internal.size graph - 1)]
                        solutions <- mapM (const solve) ([1..10000] :: [Integer])
                        let solution = foldr (Internal.shorterPath graph) (head solutions) (tail solutions)
                        self <- getSelfPid
                        case Internal.pathLength graph solution of
                          Just distance -> send warrior $ Candidate self distance solution
                          Nothing       -> send warrior $ Candidate self 0 []
                        workerLoop state

                    , match $ \Terminate ->
                        return ()
                    ]

remotable ['worker]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> External.Graph -> Process ()
run queen scheduler client graph = do
  send queen $ StrMsg "New Warrior up!"
  let graph' = Internal.mkGraphFromExternalGraph graph
  self <- getSelfPid
  let task = Task ($(mkClosure 'worker) (self :: Warrior, graph'))
  let taskCount = Internal.size graph' `div` 25 + 1
  forM_ [1..taskCount] $ \_ -> send scheduler $ WTaskS self task
  loop $ WarriorS taskCount []
    where
      loop :: WarriorS -> Process ()
      loop state@(WarriorS {..}) =
        receiveWait [ match $ \(Register workerPid) -> do
                        send queen $ StrMsg $ "A worker registered: " ++ show workerPid
                        send workerPid Run
                        loop state

                    , match $ \(Candidate workerPid int path) -> do
                        send workerPid Terminate
                        send queen $ StrMsg "Got a solution from a worker..."
                        loop $ state { solutions = (int, path) : solutions }

                    , match $ \s@(StrMsg _) -> do
                        send queen s
                        loop state

                    , matchIf (\_ -> (fromIntegral . length $ solutions) == taskCount) $ \SSendSolutionW -> do
                        let (value, solution) = minimum solutions
                        send client $ SSolutionC $ Solution (pack . show $ solution) value
                        return ()
                    ]