{-# LANGUAGE TemplateHaskell, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.TSP
  ( __remoteTable
  , interpret
  , mkConfiguration
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process              (liftIO, say)
import Control.Distributed.Process.Closure      (mkClosure, mkStatic, remotable)
import Control.Distributed.Process.Serializable (SerializableDict(SerializableDict))

import System.Random (randomRIO)

import Data.List     ((\\))

import Hive.Interface
import Hive.Imports.MkBinary
import Hive.Data.Graph
import Hive.Problem.TSP.Pheromones

import qualified Control.Distributed.Process as CH (Process)

-------------------------------------------------------------------------------

data Configuration = Configuration { graph      :: Graph Int
                                   , ants       :: Ants
                                   , iterations :: Iterations
                                   , alpha      :: Alpha
                                   , beta       :: Beta
                                   }
  deriving (Generic, Typeable)

instance Binary Configuration where

type Ants       = Int
type Iterations = Int
type Alpha      = Double
type Beta       = Double
type Visited    = [Node]
type Unvisited  = [Node]

type Result     = (Path, Pheromones)

-------------------------------------------------------------------------------

mkConfiguration :: Graph Int -> Ants -> Iterations -> Alpha -> Beta -> Configuration
mkConfiguration = Configuration

-------------------------------------------------------------------------------

ant :: (Configuration, Pheromones) -> CH.Process Path
ant (Configuration {..}, pheromones) = do
  runAnt graph pheromones [1] (nodes graph \\ [1])
  where
    runAnt :: Graph Int -> Pheromones -> Visited -> Unvisited -> CH.Process Path
    runAnt _ _ visited        [] = return visited
    runAnt g p visited unvisited = do
      let tau   = distance' p (last visited)
      let eta   = (1.0/) . fromIntegral . distance' g (last visited)
      let probs = [tau u**alpha * eta u**beta | u <- unvisited]
      rand <- liftIO $ randomRIO (0, sum probs)
      let next  = fst . head . dropWhile ((< rand) . snd) $ zip unvisited (scanl1 (+) probs)
      runAnt g p (visited ++ [next]) (unvisited \\ [next])

combineResults :: (Graph Int, Result, Result) -> CH.Process Result
combineResults (g, (pa1, ph1), (pa2, ph2)) = return (shorterPath g pa1 pa2, overlay (+) ph1 ph2)

pathDict :: SerializableDict Path
pathDict = SerializableDict

resultDict :: SerializableDict Result
resultDict = SerializableDict

-------------------------------------------------------------------------------

remotable ['ant, 'combineResults, 'pathDict, 'resultDict]

-------------------------------------------------------------------------------

antProcess :: Process (Configuration, Pheromones) Path
antProcess = mkSimple $(mkStatic 'pathDict) $(mkClosure 'ant)

-------------------------------------------------------------------------------

interpret :: Configuration -> Process (Configuration, Pheromones) Path
interpret _ = antProcess
