{-# LANGUAGE TemplateHaskell #-}

module Hive.Problem.Data.External.Graph
  where

import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Aeson              (decode)
import Data.Aeson.TH           (deriveJSON, defaultOptions)

-------------------------------------------------------------------------------

type Node     = Int
type Distance = Integer
type Edge     = (Node, Node, Distance)
type Position = (Int, Int)
type DistanceEntry = (Node, [(Node,Distance)])

data Graph = Graph { nodes :: [Node]
                   , edges :: [Edge]
                   }
           | PosList { positions :: [(Node, Position)]
                     }
           | DistanceList { distances :: [DistanceEntry]
                          }
  deriving (Show, Eq)

-------------------------------------------------------------------------------

$(deriveJSON defaultOptions ''Graph)

-------------------------------------------------------------------------------

size :: Graph -> Int
size (Graph        ns _) = length ns
size (PosList      ps  ) = length ps
size (DistanceList dl  ) = length dl

parse :: Text -> Maybe Graph
parse = decode . encodeUtf8