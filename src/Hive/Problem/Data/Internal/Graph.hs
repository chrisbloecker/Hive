module Hive.Problem.Data.Internal.Graph
  ( Graph
  , Path
  , Node
  , mkDirectedGraph
  , mkDirectedGraphFromExternalGraph
  , size
  , nodes
  , distance
  , addEdge
  , updateEdge
  , removeEdge
  , neighbours
  , pathLength
  , shorterPath
  ) where

import Data.List           (nub)
import Data.Map            ( Map
                           , empty
                           , insert
                           , delete
                           , filterWithKey
                           , keys
                           )
import Control.Applicative ( Applicative
                           , (<$>)
                           , (<*>)
                           )

-------------------------------------------------------------------------------

import qualified Data.Map                         as M
import qualified Hive.Problem.Data.External.Graph as E (Graph (..))

-------------------------------------------------------------------------------

type Size     = Integer
type Node     = Integer
type Distance = Integer
type Path     = [Node]
type Matrix   = Map (Node, Node) Distance

data Graph = DirectedGraph Size Matrix
  deriving (Show)

-------------------------------------------------------------------------------

(<+>) :: (Applicative a, Num n) => a n -> a n -> a n
m <+> n = (+) <$> m <*> n

mkDirectedGraph :: Size -> Graph
mkDirectedGraph s = DirectedGraph s empty

mkDirectedGraphFromExternalGraph :: E.Graph -> Graph
mkDirectedGraphFromExternalGraph (E.Graph v e) =
  let g = mkDirectedGraph (fromIntegral . length $ v)
  in  foldr (\(from,to,val) g' -> addEdge g' from to val) g e

size :: Graph -> Size
size (DirectedGraph s _) = s

nodes :: Graph -> [Node]
nodes (DirectedGraph _ m) = nub . concatMap (\(f,s) -> [f,s]) . keys $ m

distance :: Graph -> Node -> Node -> Maybe Distance
distance (DirectedGraph _ m) from to = (from, to) `M.lookup` m

addEdge :: Graph -> Node -> Node -> Distance -> Graph
addEdge (DirectedGraph s m) from to d = DirectedGraph s (insert (from, to) d m)

updateEdge :: Graph -> Node -> Node -> Distance -> Graph
updateEdge g@(DirectedGraph _ _) = addEdge g

removeEdge :: Graph -> Node -> Node -> Graph
removeEdge (DirectedGraph s m) from to = DirectedGraph s ((from, to) `delete` m)

neighbours :: Graph -> Node -> [Node]
neighbours (DirectedGraph _ m) from = map snd . M.keys $ filterWithKey (\k _ -> fst k == from) m

pathLength :: Graph -> Path -> Maybe Distance
pathLength (DirectedGraph _ m) p = foldr ((<+>) . (`M.lookup` m)) (Just 0) $ p `zip` tail p

shorterPath :: Graph -> Path -> Path -> Path
shorterPath g p1 p2 = shorterPath' (p1, f p1) (p2, f p2)
  where
    f :: Path -> Maybe Distance
    f = pathLength g

    shorterPath' :: (Path, Maybe Distance) -> (Path, Maybe Distance) -> Path
    shorterPath' (p1', Nothing) (_,   Nothing) = p1'
    shorterPath' (p1', Just _ ) (_,   Nothing) = p1'
    shorterPath' (_,   Nothing) (p2', Just _ ) = p2'
    shorterPath' (p1', Just l1) (p2', Just l2) | l1 <= l2   = p1'
                                               | otherwise  = p2'