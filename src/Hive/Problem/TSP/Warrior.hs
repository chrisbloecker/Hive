{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.TSP.Warrior
  ( run
  , __remoteTable
  ) where

import Control.Distributed.Process         (Process, send, getSelfPid, expect, liftIO)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Control.Monad     (forM_)
import Control.Arrow     ((&&&))

import Data.Function     (on)
import Data.Foldable     (foldr')
import Data.List         ((\\), minimumBy)
import Data.Text.Lazy    (pack)
import Data.Binary       (Binary, put, get)
import Data.Typeable     (Typeable)
import Data.DeriveTH     (derive, makeBinary)
import GHC.Generics      (Generic)

import System.Random     (randomRIO)

import Hive.Types            (Queen, Warrior, Scheduler, Client, Task (..), Solution (..))
import Hive.Messages         (SSolutionC (..), WTaskS (..))

import Hive.Problem.Data.Graph (Graph, Path, Node, size, pathLength', nodes, distance', overlay)
import Hive.Problem.TSP.Pheromones (Pheromones, mkPheromones, evaporation, depositPheromones)

-------------------------------------------------------------------------------

type Iteration   = Int
type Ants        = Int

data Candidates = Candidates [(Path, Int)] Pheromones     deriving (Generic, Typeable, Show)

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
  candidates <- mapM (const (runAnt graph pheromones [1] (nodes graph \\ [1]))) [1..ants]
  let candidates' = map ((id &&& pathLength' graph) . (++ [1])) $! candidates
  send warrior $ Candidates candidates' (depositPheromones candidates' (mkPheromones graph 0.0))
    where
      runAnt :: Graph Int -> Pheromones -> [Node] -> [Node] -> Process Path
      runAnt _ _ visited        [] = return visited
      runAnt g p visited unvisited = do
        let alpha    = 3
        let beta     = 5
        let tau      = distance' p (last visited)
        let eta      = (1.0/) . fromIntegral . distance' g (last visited)
        let probs    = [tau u**alpha * eta u**beta | u <- unvisited]
        rand <- liftIO $ randomRIO (0, sum probs)
        let next  = fst . head . dropWhile ((< rand) . snd) $ zip unvisited (scanl1 (+) probs)
        runAnt g p (visited ++ [next]) (unvisited \\ [next])


remotable ['ant]

-------------------------------------------------------------------------------

run :: Queen -> Scheduler -> Client -> Graph Int -> Iteration -> Process ()
run queen scheduler client g iterations = do
  let initialSolution = nodes g
  loop $ WarriorS g (mkPheromones g 1.0) (initialSolution, pathLength' g initialSolution) 0
    where
      loop :: WarriorS -> Process ()
      loop state@(WarriorS {..}) =
        if iteration < iterations then do
          self <- getSelfPid
          let task = Task ($(mkClosure 'ant) (self :: Warrior, graph, pheromones, size graph))
          forM_ [1 .. (size graph)] $ \_ -> send scheduler $ WTaskS self task
          candidates <- mapM (\_ -> do { Candidates trips pheromones' <- expect; return (trips, pheromones') }) [1 .. (size graph)]
          let trips       = concat . fst . unzip $ candidates
          let pheromones' = snd . unzip $ candidates
          loop state { pheromones = foldr' (overlay (+)) (evaporation 0.1 pheromones) pheromones'
                     , solution   = minimumBy (compare `on` snd) (solution : trips)
                     , iteration  = iteration + 1 }
        else
            send client $ SSolutionC $ Solution (pack . show . fst $ solution) (snd solution)