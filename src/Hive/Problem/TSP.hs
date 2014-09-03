{-# LANGUAGE TemplateHaskell, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.TSP
  ( __remoteTable
  , interpret
  , mkConfiguration
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process              (liftIO)
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
                                   , tau        :: Tau
                                   , eta        :: Eta
                                   }
  deriving (Generic, Typeable)

instance Binary Configuration where

type Ants       = Int
type Iterations = Int
type Alpha      = Double
type Beta       = Double
type Tau        = Double
type Eta        = Double
type Visited    = [Node]
type Unvisited  = [Node]

-------------------------------------------------------------------------------

mkConfiguration :: Graph Int -> Ants -> Iterations -> Alpha -> Beta -> Tau -> Eta -> Configuration
mkConfiguration = Configuration

-------------------------------------------------------------------------------

ant :: (Configuration, Pheromones) -> CH.Process Path
ant (Configuration {..}, pheromones) = runAnt graph pheromones [1] (nodes graph \\ [1])
  where
    runAnt :: Graph Int -> Pheromones -> Visited -> Unvisited -> CH.Process Path
    runAnt _ _ visited        [] = return visited
    runAnt g p visited unvisited = do
      let alpha    = 3
      let beta     = 5
      let tau      = distance' p (last visited)
      let eta      = (1.0/) . fromIntegral . distance' g (last visited)
      let probs    = [tau u**alpha * eta u**beta | u <- unvisited]
      rand <- liftIO $ randomRIO (0, sum probs)
      let next  = fst . head . dropWhile ((< rand) . snd) $ zip unvisited (scanl1 (+) probs)
      runAnt g p (visited ++ [next]) (unvisited \\ [next])

cycle :: Configuration -> CH.Process Path
cycle = undefined {-let initialSolution = nodes g
  loop $ WarriorS g (mkPheromones g 1.0) (initialSolution, pathLength' g initialSolution) 0
    where
      loop :: WarriorS -> Process ()
      loop state@(WarriorS {..}) =
        if iteration < iterations then do
          self <- getSelfPid
          let task = Task ($(mkClosure 'ant) (self :: Warrior, graph, pheromones))
          forM_ [1 .. (size graph)] $ \_ -> send scheduler $ WTaskS self task
          candidates <- mapM (\_ -> do { Candidate trip <- expect; return trip }) [1 .. (size graph)]
          loop state { pheromones = depositPheromones candidates . evaporation 0.1 $ pheromones
                     , solution   = minimumBy (compare `on` snd) (solution : candidates)
                     , iteration  = iteration + 1 }
        else
            send client $ SSolutionC $ Solution (pack . show . fst $ solution) (snd solution)
-}
pathDict :: SerializableDict Path
pathDict = SerializableDict

-------------------------------------------------------------------------------

remotable ['ant, 'cycle, 'pathDict]

-------------------------------------------------------------------------------

antProcess :: Process (Configuration, Pheromones) Path
antProcess = mkSimple $(mkStatic 'pathDict) $(mkClosure 'ant)

cycleProcess :: Process Configuration Path
cycleProcess = mkSimple $(mkStatic 'pathDict) $(mkClosure 'cycle)

-------------------------------------------------------------------------------

interpret :: Configuration -> Process (Graph Int) Path
interpret = undefined