{-# LANGUAGE TemplateHaskell, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.TSP
  ( Configuration (..)
  , __remoteTable
  , interpret
  ) where

-------------------------------------------------------------------------------

import Control.Arrow ((&&&), second)

import Control.Distributed.Process              (liftIO, say)
import Control.Distributed.Process.Closure      (mkClosure, mkStatic, remotable)
import Control.Distributed.Process.Serializable (SerializableDict(SerializableDict))

import System.Random (randomRIO)

import Data.Function   (on)
import Data.List       ((\\), minimumBy)
import Data.List.Split (chunksOf)
import Data.Maybe      (fromMaybe, isJust)

import Hive.Interface
import Hive.Imports.MkBinary
import Hive.Data.Graph
import Hive.Problem.TSP.Pheromones

import qualified Control.Distributed.Process as CH (Process)

-------------------------------------------------------------------------------

data Configuration = Configuration { graph      :: !(Graph Int)
                                   , pheromones :: !Pheromones
                                   , path       :: !Path
                                   , pathLen    :: !Int
                                   , ants       :: !Ants
                                   , iterations :: !Iterations
                                   , alpha      :: !Alpha
                                   , beta       :: !Beta
                                   , rho        :: !Rho
                                   }
  deriving (Generic, Typeable)

instance Binary Configuration where

type Ants        = Int
type Iterations  = Int
type Alpha       = Double
type Beta        = Double
type Rho         = Double
type Visited     = [Node]
type Unvisited   = [Node]
type AntSolution = (Path, Int)

-------------------------------------------------------------------------------

ant :: Configuration -> BasicProcess AntSolution
ant Configuration {..} = do
  path <- runAnt graph pheromones [1] (nodes graph \\ [1])
  return (path, pathLength' graph path)
    where
      runAnt :: Graph Int -> Pheromones -> Visited -> Unvisited -> BasicProcess Path
      runAnt _ _ visited        [] = return visited
      runAnt g p visited unvisited = do
        let tau   = distance' p (last visited)
        let eta   = (1.0/) . fromIntegral . distance' g (last visited)
        let probs = [tau u**alpha * eta u**beta | u <- unvisited]
        rand <- liftIO $ randomRIO (0, sum probs)
        let next  = fst . head . dropWhile ((< rand) . snd) $ zip unvisited (scanl1 (+) probs)
        runAnt g p (visited ++ [next]) (unvisited \\ [next])

combinePaths :: (Configuration, [AntSolution]) -> BasicProcess Configuration
combinePaths (conf@(Configuration {..}), ass) = return conf { pheromones = pheromones', path = path', pathLen = len' }
  where
    pheromones'   = depositPheromones ass pheromones
    (path', len') = minimumBy (compare `on` snd) ass

evaporations :: Configuration -> BasicProcess Configuration
evaporations (conf@Configuration {..}) = return conf { pheromones = evaporation rho pheromones }

iter :: Configuration -> BasicProcess Configuration
iter conf@Configuration {..} = return conf { iterations = iterations - 1 }

continueIter :: Configuration -> BasicProcess Bool
continueIter Configuration {..} = return (iterations > 0)

extractSolution :: Configuration -> BasicProcess Path
extractSolution (Configuration {..}) = return path

-------------------------------------------------------------------------------

boolDict :: SerializableDict Bool
boolDict = SerializableDict

pathDict :: SerializableDict Path
pathDict = SerializableDict

antSolutionDict :: SerializableDict AntSolution
antSolutionDict = SerializableDict

configurationDict :: SerializableDict Configuration
configurationDict = SerializableDict

-------------------------------------------------------------------------------

remotable ['ant
          , 'combinePaths
          , 'evaporations
          , 'iter
          , 'continueIter
          , 'extractSolution
          , 'boolDict
          , 'pathDict
          , 'antSolutionDict
          , 'configurationDict
          ]

-------------------------------------------------------------------------------

antProcess :: Process Configuration AntSolution
antProcess = Simple $(mkStatic 'antSolutionDict) $(mkClosure 'ant)

combinePathsProcess :: Process (Configuration, [AntSolution]) Configuration
combinePathsProcess = Simple $(mkStatic 'configurationDict) $(mkClosure 'combinePaths)

evaporationProcess :: Process Configuration Configuration
evaporationProcess = Simple $(mkStatic 'configurationDict) $(mkClosure 'evaporations)

iterProcess :: Process Configuration Configuration
iterProcess = Simple $(mkStatic 'configurationDict) $(mkClosure 'iter)

continueIterProcess :: Predicate Configuration
continueIterProcess = Simple $(mkStatic 'boolDict) $(mkClosure 'continueIter)

extractSolutionProcess :: Process Configuration Path
extractSolutionProcess = Simple $(mkStatic 'pathDict) $(mkClosure 'extractSolution)

-------------------------------------------------------------------------------

interpret :: Configuration -> Process Configuration Path
interpret conf@Configuration {..} = do
  let antRuns   = Multilel (replicate ants antProcess) Id (Local combinePathsProcess)
      innerProc = antRuns `Sequence` Local evaporationProcess `Sequence` Local iterProcess
      loop      = Repetition continueIterProcess innerProc
  Sequence loop (Local extractSolutionProcess)
