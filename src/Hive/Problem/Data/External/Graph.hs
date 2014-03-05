{-# LANGUAGE TemplateHaskell #-}

module Hive.Problem.Data.External.Graph
  where

import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Aeson
import Data.Aeson.TH

-------------------------------------------------------------------------------

type Node     = Integer
type Distance = Integer
type Edge     = (Node, Node, Distance)

data Graph = Graph { nodes :: [Node]
                   , edges :: [Edge]
                   }
  deriving (Show, Eq)

-------------------------------------------------------------------------------

$(deriveJSON defaultOptions ''Graph)

-------------------------------------------------------------------------------

parse :: Text -> Maybe Graph
parse = decode . encodeUtf8