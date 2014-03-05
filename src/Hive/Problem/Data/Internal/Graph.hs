module Graph
  ( Graph
  , mkDirectedGraph
  , size
  , distance
  , addEdge
  , updateEdge
  , removeEdge
  , neighbours
  , pathLength
  ) where

import Data.Map (Map, empty, insert, delete, filterWithKey)
import Control.Applicative (Applicative, (<$>), (<*>))

import qualified Data.Map as M

-------------------------------------------------------------------------------

type Size     = Integer
type Node     = Integer
type Distance = Integer
type Path     = [Node]
type Matrix   = Map (Node, Node) Distance

data Graph = DirectedGraph Size Matrix
  deriving (Show)

-------------------------------------------------------------------------------

--instance FromJSON Graph where
  --parseJSON 

-------------------------------------------------------------------------------

(<+>) :: (Applicative a, Num n) => a n -> a n -> a n
m <+> n = (+) <$> m <*> n

mkDirectedGraph :: Size -> Graph
mkDirectedGraph s = DirectedGraph s empty

size :: Graph -> Size
size (DirectedGraph s _) = s

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