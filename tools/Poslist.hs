{-# LANGUAGE TemplateHaskell #-}

module Poslist
  ( Poslist
  , parse
  , convertToGraph
  ) where

import DeriveJSON

import Data.Foldable           (foldr')

import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Graph (Graph, mkEmptyGraph, addEdge)

-------------------------------------------------------------------------------

type X        = Double
type Y        = Double
type Distance = Int
type Pos      = (X, Y)
data Poslist  = Poslist ![Pos]  deriving (Show)

-------------------------------------------------------------------------------

$(deriveJSON hiveJSONOptions ''Poslist)

-------------------------------------------------------------------------------

convertToGraph :: Poslist -> Graph Int
convertToGraph (Poslist pl) =
  let pss = zip [1..] . map (zip [1..] . zipWith dist pl . repeat) $ pl
  in  foldr' (\(f, ts) g -> foldr' (\(t, d) g' -> addEdge g' (f, t, d)) g ts) mkEmptyGraph pss
    where
      dist :: Pos -> Pos -> Distance
      dist (x1, y1) (x2, y2) = round . sqrt $ (x1-x2)**2 + (y1-y2)**2

-------------------------------------------------------------------------------

parse :: Text -> Maybe Poslist
parse = decode' . encodeUtf8
