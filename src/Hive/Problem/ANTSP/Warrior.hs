{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.ANTSP.Warrior
  ( run
  , __remoteTable
  ) where

import Control.Distributed.Process         (Process, send, getSelfPid, expect, liftIO)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Control.Monad     (forM_)
import Control.Arrow     ((&&&))

import Data.Function     (on)
import Data.List         ((\\), minimumBy)
import Data.Text.Lazy    (pack)
import Data.Binary       (Binary, put, get)
import Data.Typeable     (Typeable)
import Data.DeriveTH     (derive, makeBinary)
import GHC.Generics      (Generic)

import System.Random     (randomRIO)

import Hive.Types            (Queen, Warrior, Scheduler, Client, Task (..), Solution (..))
import Hive.Messages         (SSolutionC (..), WTaskS (..))

import Hive.Problem.Data.Graph (Graph, Path, Node, size, pathLength', nodes, distance')
import Hive.Problem.ANTSP.Pheromones (Pheromones, mkPheromones, evaporation, depositPheromones)

-------------------------------------------------------------------------------

type Iteration   = Int
type Ants        = Int

newtype Candidates = Candidates [(Path, Int)]     deriving (Generic, Typeable, Show)

data WarriorS = WarriorS { graph      :: Graph Int
                         , pheromones :: Pheromones
                         , solution   :: (Path, Int)
                         , iteration  :: Iteration
                         } deriving (Eq, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''Candidates)

-------------------------------------------------------------------------------

ant :: (Warrior, Graph Int, Pheromones, Ants) -> Process ()
ant (warrior, graph, pheromones, ants) = do
  candidates <- liftIO $ mapM (const (runAnt graph pheromones [1] (nodes graph \\ [1]))) [1..ants]
  send warrior $ Candidates $ map (id &&& pathLength' graph) candidates
    where
      runAnt :: Graph Int -> Pheromones -> [Node] -> [Node] -> IO Path
      runAnt _ _ visited        [] = return visited
      runAnt g p visited unvisited = do
        let alpha    = 3
            beta     = 5
            tau      = distance' p (last visited)
            eta      = fromIntegral . distance' g (last visited)
            probs    = [tau u**alpha * eta u**beta | u <- unvisited]
        rand <- randomRIO (0, sum probs)
        let next  = fst . last . takeWhile ((< rand) . snd) $ zip unvisited (scanl1 (+) probs)
        runAnt g p (visited ++ [next]) (unvisited \\ [next])


remotable ['ant]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> Graph Int -> Iteration -> Process ()
run queen scheduler client g iterations = do
  let initialSolution = nodes g
  loop $ WarriorS g (mkPheromones g) (initialSolution, pathLength' g initialSolution) 0
    where
      loop :: WarriorS -> Process ()
      loop state@(WarriorS {..}) =
        if iteration < iterations then do
          self <- getSelfPid
          let task = Task ($(mkClosure 'ant) (self :: Warrior, graph, pheromones, size graph))
          forM_ [1 .. (size graph)] $ \_ -> send scheduler $ WTaskS self task
          candidateMessages <- mapM (\_ -> do { Candidates trips <- expect; return trips }) [1 .. (size graph)]
          let candidates = concat candidateMessages
          loop state { pheromones = depositPheromones candidates . evaporation 0.1 $ pheromones
                     , solution   = minimumBy (compare `on` snd) (solution : candidates)
                     , iteration  = iteration + 1 }
        else
            send client $ SSolutionC $ Solution (pack . show . fst $ solution) (snd solution)