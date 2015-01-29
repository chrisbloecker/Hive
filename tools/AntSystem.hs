{-# LANGUAGE RecordWildCards #-}

module Main
  where

-------------------------------------------------------------------------------

import           Data.Function            (on)
import           System.Environment       (getArgs)
import           Control.Arrow            ((&&&), second)
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

import           Computation

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

type Ants       = Int
type Iterations = Int
type Alpha      = Double
type Beta       = Double
type Rho        = Double
type Visited    = [Node]
type Unvisited  = [Node]
type AntSolution = (Path, Int)

-------------------------------------------------------------------------------

ant :: Configuration -> IO AntSolution
ant conf@(Configuration {..}) = do
  path <- runAnt graph pheromones [1] (nodes graph \\ [1])
  return (path, pathLength' graph path)
    where
      runAnt :: Graph Int -> Pheromones -> Visited -> Unvisited -> IO Path
      runAnt _ _ visited        [] = return visited
      runAnt g p visited unvisited = do
        let tau   = distance' p (last visited)
        let eta   = (1.0/) . fromIntegral . distance' g (last visited)
        let probs = [tau u**alpha * eta u**beta | u <- unvisited]
        rand <- randomRIO (0, sum probs)
        let next  = fst . head . dropWhile ((< rand) . snd) $ zip unvisited (scanl1 (+) probs)
        runAnt g p (visited ++ [next]) (unvisited \\ [next])

combinePaths :: (Configuration, [AntSolution]) -> IO Configuration
combinePaths (conf@(Configuration {..}), ps) = return conf { pheromones = pheromones', path = path', pathLen = len' }
  where
    pheromones'   = depositPheromones ps pheromones
    (path', len') = minimumBy (compare `on` snd) ps

evaporations :: Configuration -> IO Configuration
evaporations (conf@Configuration {..}) = return conf { pheromones = evaporation rho pheromones }

extractSolution :: Configuration -> IO AntSolution
extractSolution (Configuration {..}) = return (path, pathLen)

-------------------------------------------------------------------------------

antC :: Computation Configuration AntSolution
antC = Simple ant

combinePathsC :: Computation (Configuration, [AntSolution]) Configuration
combinePathsC = Simple combinePaths

evaporationsC :: Computation Configuration Configuration
evaporationsC = Simple evaporations

extractSolutionC :: Computation Configuration AntSolution
extractSolutionC = Simple extractSolution

-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  case args of
    [jsonFile, antNumber, iterNumber] -> do
      fileContent <- T.readFile jsonFile
      let mposlist = P.parse fileContent
          mgraph   = maybe Nothing (Just . P.convertToGraph) mposlist
          ants     = read antNumber :: Int
          iters    = read iterNumber :: Int
      case mgraph of
        Nothing    -> print "the file didn't look good..."
        Just graph -> do
          let conf        = Configuration graph (mkPheromones graph 2) (nodes graph) (pathLength' graph $ nodes graph) ants iters 2 5 0.1
              computation = interpret conf
          solution <- runComputation computation conf
          print solution

    _ -> putStrLn "wrong args, give me one input file."

interpret :: Configuration -> Computation Configuration AntSolution
interpret conf@Configuration{..} = do
  let antRuns    = Multilel (replicate ants antC) conf combinePathsC
      innerComp  = Sequence antRuns evaporationsC
      loop       = Loop conf 0 (<iterations) id (\_ i -> i+1) innerComp
  Sequence loop extractSolutionC
