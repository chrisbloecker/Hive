{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hive.Problem.Data.Internal.Graph
  ( Graph
  , Path
  , Node
  , mkDirectedGraph
  , mkGraphFromExternalGraph
  , (<+>)
  , size
  , nodes
  , distance
  , addEdge
  , updateEdge
  , neighbours
  , pathLength
  , shorterPath
  , partition
  ) where

import Data.Binary         (Binary, get, put, putWord8, getWord8)
import Data.DeriveTH       (derive, makeBinary)
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)

import Data.List           ((\\))
import Data.IntMap.Strict  (IntMap)
import Data.Vector         (Vector)
import Data.Vector.Binary  ()
import Control.Applicative (Applicative, (<$>), (<*>))

-------------------------------------------------------------------------------

import qualified Data.Vector                      as Vector   ((!), length, toList, fromList, filter)
import qualified Data.IntMap.Strict               as Map      (empty, singleton, keys, filterWithKey, lookup, insertWith, union, size)
import qualified Hive.Problem.Data.External.Graph as External (Graph (..), size)

-------------------------------------------------------------------------------

type Size     = Int
type Node     = Int
type Distance = Int
type Path     = [Node]
type Position = (Int, Int)
type Matrix   = IntMap (IntMap Distance)

data Graph = DirectedGraph Size Matrix
           | PositionList  (Vector Position)
  deriving (Eq, Show, Generic, Typeable)

-------------------------------------------------------------------------------

$(derive makeBinary ''Graph)

-------------------------------------------------------------------------------

(<+>) :: (Applicative a, Num n) => a n -> a n -> a n
m <+> n = (+) <$> m <*> n

mkDirectedGraph :: Size -> Graph
mkDirectedGraph s = DirectedGraph s Map.empty

mkGraphFromExternalGraph :: External.Graph -> Graph
mkGraphFromExternalGraph graph@(External.Graph _ e) =
  let g = mkDirectedGraph (External.size graph)
  in  foldr (\(from,to,val) g' -> addEdge g' from to val) g e
mkGraphFromExternalGraph (External.PosList ps) =
  PositionList . Vector.fromList . map snd $ ps
mkGraphFromExternalGraph graph@(External.DistanceList dl) =
  let g = mkDirectedGraph (External.size graph)
  in  foldr (\(from,toList) g' -> foldr (\(to,d) g'' -> addEdge g'' from to d) g' toList) g dl

size :: Graph -> Size
size (DirectedGraph s _) = s
size (PositionList  ps ) = fromIntegral . Vector.length $ ps

nodes :: Graph -> [Node]
nodes (DirectedGraph  s _) = [1..s]
nodes (PositionList  ps  ) = map fst . Vector.toList $ ps

distance :: Graph -> Node -> Node -> Maybe Distance
distance (DirectedGraph  _ m) from to = from `Map.lookup` m >>= \m' -> to `Map.lookup` m'
distance (PositionList  ps  ) from to =
  let (x1, y1) = ps Vector.! fromIntegral from
      (x2, y2) = ps Vector.! fromIntegral to
  in  Just . round . sqrt . fromIntegral $ ((x1-x2)^2 + (y1-y2)^2)

addEdge :: Graph -> Node -> Node -> Distance -> Graph
addEdge (DirectedGraph s m) from to d = DirectedGraph s (Map.insertWith Map.union from (Map.singleton to d) m) -- ToDo: something more intelligent than union
addEdge (PositionList  _  ) _    _  _ = error "This graph is complete, you cannot add an edge."

updateEdge :: Graph -> Node -> Node -> Distance -> Graph
updateEdge g@(DirectedGraph _ _) = addEdge g
updateEdge g@(PositionList  _  ) = addEdge g

neighbours :: Graph -> Node -> [Node]
neighbours   (DirectedGraph _ m) from =
  case from `Map.lookup` m of
    Just m' -> Map.keys m'
    Nothing -> []
neighbours g@(PositionList  _  ) from = nodes g \\ [from]

pathLength :: Graph -> Path -> Maybe Distance
pathLength g p = foldr ((<+>) . uncurry (distance g)) (Just 0) $ p `zip` tail p

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

partition :: Graph -> Node -> Node -> Graph
partition (DirectedGraph _ m) n0 n1 =
  let m' = Map.filterWithKey (\k _ -> k <= n1 && k > n0) m
  in  DirectedGraph (Map.size m') m'
partition (PositionList ps) n0 n1 = PositionList $ Vector.filter (\(x,_) -> x > n0 && x <= n1) ps