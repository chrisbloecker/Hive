{-# LANGUAGE TemplateHaskell #-}

module Hive.Problem.TSP
  ( __remoteTable
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process.Closure      (mkClosure, mkStatic, remotable)
import Control.Distributed.Process.Serializable (SerializableDict(SerializableDict))

import Hive.Interface

import Data.Graph

import qualified Control.Distributed.Process as CH (Process)

-------------------------------------------------------------------------------

newtype Configuration = Configuration { unConfiguration :: Int } deriving (Eq, Show)
type Pheromones = Graph
type Path       = [Int]

-------------------------------------------------------------------------------

ant :: (Graph, Pheromones) -> CH.Process Path
ant (g, p) = undefined

pathDict :: SerializableDict Path
pathDict = SerializableDict

-------------------------------------------------------------------------------

remotable ['ant, 'pathDict]

-------------------------------------------------------------------------------

antProcess :: Process (Graph, Pheromones) Path
antProcess = mkSimple $(mkStatic 'pathDict) $(mkClosure 'ant)

-------------------------------------------------------------------------------

interpret :: Configuration -> Process Graph Path
interpret = undefined