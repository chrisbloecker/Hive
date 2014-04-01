module Hive.Problem.ANTSP.Pheromones
  ( Pheromones
  , mkPheromones
  , toPheromones
  , evaporation
  , depositPheromones
  ) where

import Data.Foldable (foldr')

import Hive.Problem.Data.Graph (Graph, Path, mkEmptyGraph, size, addEdge, distance', overlay)

-------------------------------------------------------------------------------

type Pheromones = Graph Double

-------------------------------------------------------------------------------

mkPheromones :: Num a => Graph a -> Pheromones
mkPheromones gr =
  let ns = [1 .. (size gr)]
  in  foldr' (\from g' -> foldr' (\to g'' -> addEdge g'' (from, to, 1.0)) g' ns) (mkEmptyGraph :: Graph Double) ns

toPheromones :: Path -> Int -> Pheromones
toPheromones rt v = foldr (\(f, t) p -> addEdge p (f, t, 1.0 / fromIntegral v)) mkEmptyGraph (zip rt (tail rt))

evaporation :: Double -> Pheromones -> Pheromones
evaporation c p =
  let ns = [1 .. (size p)]
  in  foldr' (\from g' -> foldr' (\to g'' -> addEdge g'' (from, to, (1-c) * distance' p from to)) g' ns) (mkEmptyGraph :: Graph Double) ns

depositPheromones :: [(Path, Int)] -> Pheromones -> Pheromones
depositPheromones rts p = foldr' (overlay (+)) p (map (uncurry toPheromones) rts)