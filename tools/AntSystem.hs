{-# LANGUAGE RecordWildCards #-}

module Main
  where

-------------------------------------------------------------------------------

import           Data.Function            (on)
import           System.Environment       (getArgs)
import           Control.Arrow            ((&&&))
import           System.Random            (randomRIO)
import           Data.List                ((\\), minimumBy)
import           Data.Maybe               (fromMaybe, isJust)
import           Graph
import qualified Poslist             as P
import           Pheromones
import qualified Data.Text.Lazy.IO   as T (readFile)
import           Control.Concurrent       (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad            (forM)
import           Process

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

type Ants        = Int
type Iterations  = Int
type Alpha       = Double
type Beta        = Double
type Rho         = Double
type Visited     = [Node]
type Unvisited   = [Node]
type AntSolution = (Path, Int)

-------------------------------------------------------------------------------
-- basic processes

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
        rand <- randomRIO (0, sum probs)
        let next  = fst . head . dropWhile ((< rand) . snd) $ zip unvisited (scanl1 (+) probs)
        runAnt g p (visited ++ [next]) (unvisited \\ [next])

combinePaths :: (Configuration, [AntSolution]) -> BasicProcess Configuration
combinePaths (conf@(Configuration {..}), ps) = return conf { pheromones = pheromones', path = path', pathLen = len' }
  where
    pheromones'   = depositPheromones ps pheromones
    (path', len') = minimumBy (compare `on` snd) ps

evaporations :: Configuration -> BasicProcess Configuration
evaporations (conf@Configuration {..}) = return conf { pheromones = evaporation rho pheromones }

extractSolution :: Configuration -> BasicProcess AntSolution
extractSolution (Configuration {..}) = return (path, pathLen)

-------------------------------------------------------------------------------
--

antProcess :: Process Configuration AntSolution
antProcess = Simple ant

combinePathsProcess :: Process (Configuration, [AntSolution]) Configuration
combinePathsProcess = Simple combinePaths

evaporationProcess :: Process Configuration Configuration
evaporationProcess = Simple evaporations

extractSolutionProcess :: Process Configuration AntSolution
extractSolutionProcess = Simple extractSolution

-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  case args of
    [jsonFile, antNumber, iterNumber] -> do
      fileContent <- T.readFile jsonFile
      let mposlist = P.parse fileContent
          mgraph   = fmap P.convertToGraph mposlist
          ants     = read antNumber :: Int
          iters    = read iterNumber :: Int
      case mgraph of
        Nothing    -> print "the file didn't look good..."
        Just graph -> do
          let conf        = Configuration graph (mkPheromones graph 2) (nodes graph) (pathLength' graph $ nodes graph) ants iters 2 5 0.1
              computation = interpret conf
          solution <- runProcess computation conf
          print solution

    _ -> putStrLn "wrong args, give me one input file."

interpret :: Configuration -> Process Configuration AntSolution
interpret conf@Configuration {..} = do
  let antRuns   = Multilel (replicate ants antProcess) conf combinePathsProcess
      innerProc = Sequence antRuns evaporationProcess
      loop      = Loop conf 0 (<iterations) id (\_ i -> i+1) innerProc
  Sequence loop extractSolutionProcess
