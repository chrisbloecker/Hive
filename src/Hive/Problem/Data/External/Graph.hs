{-# LANGUAGE TemplateHaskell #-}

module Hive.Problem.Data.External.Graph
  where

import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Aeson              (decode)
import Data.Aeson.TH           (deriveJSON, defaultOptions)

-------------------------------------------------------------------------------

type Node     = Integer
type Distance = Integer
type Edge     = (Node, Node, Distance)
type Position = (Integer, Integer)

data Graph = Graph { nodes :: [Node]
                   , edges :: [Edge]
                   }
           | PosList { positions :: [(Node, Position)]
                     }
  deriving (Show, Eq)

-------------------------------------------------------------------------------

$(deriveJSON defaultOptions ''Graph)

-------------------------------------------------------------------------------

parse :: Text -> Maybe Graph
parse = decode . encodeUtf8