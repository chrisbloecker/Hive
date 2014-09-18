{-# LANGUAGE TemplateHaskell, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.TSP
  ( Configuration
  , __remoteTable
  , interpret
  , mkConfiguration
  ) where

-------------------------------------------------------------------------------

import Control.Arrow ((&&&), second)

import Control.Distributed.Process              (liftIO, say)
import Control.Distributed.Process.Closure      (mkClosure, mkStatic, remotable)
import Control.Distributed.Process.Serializable (SerializableDict(SerializableDict))

import System.Random (randomRIO)

import Data.List     ((\\))
import Data.Maybe    (fromMaybe, isJust)

import Hive.Interface
import Hive.Imports.MkBinary
import Hive.Data.Graph
import Hive.Problem.TSP.Pheromones

import qualified Control.Distributed.Process as CH (Process)

-------------------------------------------------------------------------------

data Configuration = Configuration { graph      :: Graph Int
                                   , pheromones :: !Pheromones
                                   , path       :: !Path
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

-------------------------------------------------------------------------------

mkConfiguration :: Graph Int -> Pheromones -> Path -> Ants -> Iterations -> Alpha -> Beta -> Configuration
mkConfiguration = Configuration

-------------------------------------------------------------------------------

ant :: Configuration -> CH.Process Path
ant conf@(Configuration {..}) = do
  say "Ant running..."
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

combinePaths :: (Configuration, [Path]) -> CH.Process Configuration
combinePaths (conf@(Configuration {..}), ps) = return conf { pheromones = pheromones', path = path' }
  where
    pheromones' = depositPheromones ( map (second (fromMaybe undefined))
                                    . filter (isJust . snd)
                                    . map (id &&& pathLength graph)
                                    $ ps) pheromones
    path'       = foldr (shorterPath graph) path ps

extractSolution :: Configuration -> CH.Process Path
extractSolution (Configuration {..}) = return path

pathDict :: SerializableDict Path
pathDict = SerializableDict

configurationDict :: SerializableDict Configuration
configurationDict = SerializableDict

-------------------------------------------------------------------------------

remotable ['ant, 'combinePaths, 'extractSolution, 'pathDict, 'configurationDict]

-------------------------------------------------------------------------------

antProcess :: Process Configuration Path
antProcess = mkSimple $(mkStatic 'pathDict) $(mkClosure 'ant)

combinePathsProcess :: Process (Configuration, [Path]) Configuration
combinePathsProcess = mkSimple $(mkStatic 'configurationDict) $(mkClosure 'combinePaths)

extractSolutionProcess :: Process Configuration Path
extractSolutionProcess = mkSimple $(mkStatic 'pathDict) $(mkClosure 'extractSolution)

-------------------------------------------------------------------------------

interpret :: Configuration -> Process Configuration Path
interpret conf@(Configuration {..}) = do
  let innerProc = mkMultilel (take ants . repeat $ antProcess) conf (mkLocal combinePathsProcess)
  let loop      = mkLoop conf 0 (<iterations) id (\_ i -> i+1) innerProc
  mkSequence loop extractSolutionProcess
