{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.Data.External.PosList
  ( parse
  , convertToGraph
  ) where

import GHC.Generics        (Generic)
import Data.Binary         (Binary, get, put)
import Data.Typeable       (Typeable)
import Data.DeriveTH       (derive, makeBinary)

import Data.Aeson          (decode')
import Data.Aeson.TH       (deriveJSON, defaultOptions)

import Data.Foldable       (foldr')

import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Hive.Problem.Data.Graph (Graph, mkEmptyGraph, addEdge)

-------------------------------------------------------------------------------

type X        = Double
type Y        = Double
type Distance = Int
type Pos      = (X, Y)
data PosList  = PosList [Pos]          deriving (Show, Generic, Typeable)

-------------------------------------------------------------------------------

$(derive makeBinary ''PosList)

$(deriveJSON defaultOptions ''PosList)

-------------------------------------------------------------------------------

convertToGraph :: PosList -> Graph Int
convertToGraph (PosList pl) =
  let pss = zip [1..] . map (zip [1..] . zipWith dist pl . repeat) $ pl
  in  foldr' (\(f, ts) g -> foldr' (\(t, d) g' -> addEdge g' (f, t, d)) g ts) mkEmptyGraph pss
    where
      dist :: Pos -> Pos -> Distance
      dist (x1, y1) (x2, y2) = round . sqrt $ (x1-x2)**2 + (y1-y2)**2

-------------------------------------------------------------------------------

parse :: Text -> Maybe PosList
parse = decode' . encodeUtf8