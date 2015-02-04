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

extractSolution :: Configuration -> BasicProcess Path
extractSolution (Configuration {..}) = return path

pathDict :: SerializableDict Path
pathDict = SerializableDict

antSolutionDict :: SerializableDict AntSolution
antSolutionDict = SerializableDict

configurationDict :: SerializableDict Configuration
configurationDict = SerializableDict

-------------------------------------------------------------------------------

remotable ['ant, 'combinePaths, 'evaporations, 'extractSolution, 'pathDict, 'antSolutionDict, 'configurationDict]

-------------------------------------------------------------------------------

antProcess :: Process Configuration AntSolution
antProcess = Simple $(mkStatic 'antSolutionDict) $(mkClosure 'ant)

combinePathsProcess :: Process (Configuration, [AntSolution]) Configuration
combinePathsProcess = Simple $(mkStatic 'configurationDict) $(mkClosure 'combinePaths)

evaporationProcess :: Process Configuration Configuration
evaporationProcess = Simple $(mkStatic 'configurationDict) $(mkClosure 'evaporations)

extractSolutionProcess :: Process Configuration Path
extractSolutionProcess = Simple $(mkStatic 'pathDict) $(mkClosure 'extractSolution)

-------------------------------------------------------------------------------

interpret :: Configuration -> Process Configuration Path
interpret conf@Configuration {..} = do
  let antRuns   = Multilel (replicate ants antProcess) conf (Local combinePathsProcess)
      innerProc = Sequence antRuns (Local evaporationProcess)
      loop      = Loop conf 0 (<iterations) id (\_ i -> i+1) innerProc
  Sequence loop (Local extractSolutionProcess)
