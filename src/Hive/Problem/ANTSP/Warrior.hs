{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, RecordWildCards #-}

module Hive.Problem.ANTSP.Warrior
  ( run
  , __remoteTable
  ) where

import Control.Distributed.Process         (Process, send, getSelfPid, expect)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Control.Monad     (forM_)
import Control.Arrow     ((&&&))

import Data.Function     (on)
import Data.Foldable     (foldr')
import Data.List         (minimumBy)
import Data.Text.Lazy    (pack)
import Data.Binary       (Binary, put, get)
import Data.Typeable     (Typeable)
import Data.DeriveTH     (derive, makeBinary)
import GHC.Generics      (Generic)

import Hive.Types            (Queen, Warrior, Scheduler, Client, Task (..), Solution (..))
import Hive.Messages         (SSolutionC (..), WTaskS (..))

import Hive.Problem.Data.Graph (Graph, Path, size, pathLength', nodes, addEdge, mkEmptyGraph, distance', overlay)

-------------------------------------------------------------------------------

type Iteration   = Int
type Roundtrip   = Path
type Pheromones  = Graph Double
type Ants        = Int

newtype Candidates = Candidates [(Roundtrip, Int)]     deriving (Generic, Typeable, Show)

data WarriorS = WarriorS { graph      :: Graph Int
                         , pheromones :: Pheromones
                         , solution   :: (Roundtrip, Int)
                         , iteration  :: Iteration
                         } deriving (Eq, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''Candidates)

-------------------------------------------------------------------------------

worker :: (Warrior, Graph Int, Pheromones, Ants) -> Process ()
worker (warrior, g, pheromones, ants) =
  send warrior $ Candidates $ map ((id &&& pathLength' g) . const (runAnt g pheromones)) [1..ants]
    where
      runAnt :: Graph Int -> Pheromones -> Roundtrip
      runAnt = undefined


remotable ['worker]

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
          let task = Task ($(mkClosure 'worker) (self :: Warrior, graph, pheromones, size graph))
          forM_ [1 .. (size graph)] $ \_ -> send scheduler $ WTaskS self task
          candidateMessages <- mapM (\_ -> do { Candidates trips <- expect; return trips }) [1 .. (size graph)]
          let candidates = concat candidateMessages
          loop state { pheromones = depositPheromones candidates . evaporation 0.1 $ pheromones
                     , solution   = minimumBy (compare `on` snd) (solution : candidates)
                     , iteration  = iteration + 1 }
        else
            send client $ SSolutionC $ Solution (pack . show . fst $ solution) (snd solution)

      mkPheromones :: Num a => Graph a -> Pheromones
      mkPheromones gr =
        let ns = [1 .. (size gr)]
        in  foldr' (\from g' -> foldr' (\to g'' -> addEdge g'' (from, to, 1.0)) g' ns) (mkEmptyGraph :: Graph Double) ns

      evaporation :: Double -> Pheromones -> Pheromones
      evaporation c p =
        let ns = [1 .. (size p)]
        in  foldr' (\from g' -> foldr' (\to g'' -> addEdge g'' (from, to, (1-c) * distance' p from to)) g' ns) (mkEmptyGraph :: Graph Double) ns

      depositPheromones :: [(Roundtrip, Int)] -> Pheromones -> Pheromones
      depositPheromones rts p = foldr' overlay p (map (uncurry toPheromones) rts)

      toPheromones :: Roundtrip -> Int -> Pheromones
      toPheromones rt v = foldr (\(f, t) p -> addEdge p (f, t, 1.0 / fromIntegral v)) mkEmptyGraph (zip rt (tail rt))