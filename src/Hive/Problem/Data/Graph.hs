{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hive.Problem.Data.Graph
  ( Graph
  , Path
  , Node
  , mkEmptyGraph
  , (<+>)
  , size
  , nodes
  , distance
  , distance'
  , addEdge
  , updateEdge
  , neighbours
  , pathLength
  , pathLength'
  , shorterPath
  , partition
  , partitions
  , overlay
  , parse
  ) where

import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Data.Binary         (Binary, get, put)
import Data.DeriveTH       (derive, makeBinary)
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)

import Data.Aeson          (FromJSON, decode')
import Data.Aeson.TH       (deriveJSON, defaultOptions)

import Data.Foldable       (foldr')
import Data.List           (unfoldr)
import Data.IntMap.Strict  (IntMap)
import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Arrow       ((&&&), first, second)

import Data.Monoid         (Monoid(mempty, mappend))

-------------------------------------------------------------------------------

import qualified Data.IntMap.Strict as M ((!), empty, singleton, keys, filterWithKey, lookup, insertWith, union, size, partitionWithKey)

-------------------------------------------------------------------------------

type Size     = Int
type Node     = Int
type Edge a   = (Node, Node, a)
type Path     = [Node]
type Matrix a = IntMap (IntMap a)

data Graph a = Graph !(Matrix a)
  deriving (Eq, Show, Generic, Typeable)

instance Monoid (Graph a) where
  mempty = Graph M.empty
  mappend = undefined

-------------------------------------------------------------------------------

$(derive makeBinary ''Graph)

$(deriveJSON defaultOptions ''Graph)

-------------------------------------------------------------------------------

(<+>) :: (Applicative a, Num n) => a n -> a n -> a n
m <+> n = (+) <$> m <*> n

mkEmptyGraph :: Num a => Graph a
mkEmptyGraph = mempty

size :: Num a => Graph a -> Size
size (Graph m) = M.size m

nodes :: Num a => Graph a -> [Node]
nodes (Graph m) = M.keys m

distance :: Num a => Graph a -> Node -> Node -> Maybe a
distance (Graph m) from to = from `M.lookup` m >>= \m' -> to `M.lookup` m'

distance' :: Num a => Graph a -> Node -> Node -> a
distance' (Graph m) from to = (m M.! from) M.! to

addEdge :: Num a => Graph a -> Edge a -> Graph a
addEdge (Graph m) (from, to, d) = Graph $ M.insertWith M.union from (M.singleton to d) m

updateEdge :: Num a => Graph a -> Edge a -> Graph a
updateEdge = addEdge

neighbours :: Num a => Graph a -> Node -> [Node]
neighbours (Graph m) from =
  case from `M.lookup` m of
    Just m' -> M.keys m'
    Nothing -> []

pathLength :: Num a => Graph a -> Path -> Maybe a
pathLength g p = foldr' ((<+>) . uncurry (distance g)) (Just 0) $ p `zip` tail p

-- | Assuming a complete graph where there will be no errors
pathLength' :: Num a => Graph a -> Path -> a
pathLength' g p = foldr' ((+) . uncurry (distance' g)) 0 $ p `zip` tail p

shorterPath :: (Ord a, Num a) => Graph a -> Path -> Path -> Path
shorterPath g p1 p2 = shorterPath' ((id &&& pathLength g) p1) ((id &&& pathLength g) p2)
  where
    shorterPath' :: (Ord a, Num a) => (Path, Maybe a) -> (Path, Maybe a) -> Path
    shorterPath' (p1', Nothing) (_,   Nothing) = p1'
    shorterPath' (p1', Just _ ) (_,   Nothing) = p1'
    shorterPath' (_,   Nothing) (p2', Just _ ) = p2'
    shorterPath' (p1', Just l1) (p2', Just l2) | l1 <= l2   = p1'
                                               | otherwise  = p2'

partition :: Num a =>  Graph a -> Node -> Node -> Graph a
partition (Graph m) n0 n1 = Graph (M.filterWithKey (\k _ -> k <= n1 && k > n0) m)

partitions :: Num a => Graph a -> Int -> Int -> [Graph a]
partitions (Graph m) parts indicator =
  let unF (m', i) = if i < parts then Just
                                    . first Graph
                                    . second (id &&& const (i+1))
                                    . M.partitionWithKey (\k _ -> k <= (i+1)*indicator)
                                    $ m'
                                 else Nothing
  in  unfoldr unF (m, 0)

overlay :: Num a => Graph a -> Graph a -> Graph a
overlay (Graph _m1) (Graph _m2) = undefined

-------------------------------------------------------------------------------

parse :: (Num a, FromJSON a) => Text -> Maybe (Graph a)
parse = decode' . encodeUtf8