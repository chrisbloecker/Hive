module Hive.Problem.TSP.Permutation
  where

import Hive.Problem.Data.Internal.Graph (Graph, Path, shorterPath)

solve :: Graph -> Path -- make it IO
solve = undefined

combine :: Graph -> [Path] -> Path
combine _    []  = []
combine _ (p:[]) = p
combine g (p:ps) = foldr (shorterPath g) p ps