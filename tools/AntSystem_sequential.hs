{-# LANGUAGE RecordWildCards #-}

module Main
  where

-------------------------------------------------------------------------------

import           System.IO                (hFlush, stdout)
import           System.Environment       (getArgs)
import           Control.Arrow            ((&&&), second)
import           System.Random            (randomRIO)
import           Data.List                ((\\))
import           Data.Maybe               (fromMaybe, isJust)
import           Graph
import qualified Poslist             as P
import           Pheromones
import qualified Data.Text.Lazy.IO   as T (readFile)
import           Control.Concurrent       (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad            (forM, void)

-------------------------------------------------------------------------------

data Configuration = Configuration { graph      :: Graph Int
                                   , pheromones :: !Pheromones
                                   , path       :: !Path
                                   , ants       :: Ants
                                   , iterations :: Iterations
                                   , alpha      :: Alpha
                                   , beta       :: Beta
                                   , rho        :: Rho
                                   }

type Ants       = Int
type Iterations = Int
type Alpha      = Double
type Beta       = Double
type Rho        = Double
type Visited    = [Node]
type Unvisited  = [Node]

-------------------------------------------------------------------------------

ant :: Configuration -> MVar Path -> IO ()
ant conf@(Configuration {..}) mvar = do
  putStr "."
  hFlush stdout
  runAnt graph pheromones [1] (nodes graph \\ [1]) mvar
    where
      runAnt :: Graph Int -> Pheromones -> Visited -> Unvisited -> MVar Path -> IO ()
      runAnt _ _ visited        [] mvar = void (putMVar mvar visited)
      runAnt g p visited unvisited mvar = do
        let tau   = distance' p (last visited)
        let eta   = (1.0/) . fromIntegral . distance' g (last visited)
        let probs = [tau u**alpha * eta u**beta | u <- unvisited]
        rand <- randomRIO (0, sum probs)
        let next  = fst . head . dropWhile ((< rand) . snd) $ zip unvisited (scanl1 (+) probs)
        runAnt g p (visited ++ [next]) (unvisited \\ [next]) mvar

combinePaths :: Configuration -> [Path] -> Configuration
combinePaths conf@Configuration{..} ps = conf { pheromones = pheromones', path = path' }
  where
    pheromones' = depositPheromones ( map (second (fromMaybe undefined))
                                    . filter (isJust . snd)
                                    . map (id &&& pathLength graph)
                                    $ ps) pheromones
    path'       = foldr (shorterPath graph) path ps

-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  case args of
    [jsonFile] -> do
      fileContent <- T.readFile jsonFile
      let mposlist = P.parse fileContent
          mgraph   = fmap P.convertToGraph mposlist
      case mgraph of
        Nothing    -> print "the file didn't look good..."
        Just graph -> do
          path <- solve (Configuration graph (mkPheromones graph 2) (nodes graph) (size graph) 100 1 3 0.1)
          print (pathLength' graph path , path)

    _ -> putStrLn "wrong args, give me one input file."

solve :: Configuration -> IO Path
solve (Configuration _ _ path _ 0 _ _ _) = return path
solve conf@Configuration{..} = do
  putStr $ (show iterations) ++ " "
  hFlush stdout
  mvars <- forM [1..ants] (const newEmptyMVar)
  mapM_ (\(a, mvar) -> a conf mvar) (repeat ant `zip` mvars)
  newPaths <- forM mvars $ \m -> takeMVar m
  let conf' = combinePaths conf newPaths
  putStrLn ""
  solve conf' { iterations = iterations - 1, pheromones = evaporation rho pheromones }