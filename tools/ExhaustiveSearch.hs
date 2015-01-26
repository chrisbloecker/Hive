module Main
  where

import           System.Environment       (getArgs)
import qualified Data.Text.Lazy.IO   as T (readFile)
import qualified Graph               as G
import qualified Poslist             as P
import           Data.List                (permutations)
import           Control.Arrow            ((&&&))

main :: IO ()
main = do
  args <- getArgs

  case args of
    [jsonFile] -> do
      json <- T.readFile jsonFile
      let mposlist = P.parse json

      case mposlist of
        Nothing       -> putStrLn "better check your file, it didn't look good..."
        Just postlist -> do
          let graph    = P.convertToGraph postlist
              nodes    = G.nodes graph
              solution = findMin . map (G.pathLength' graph &&& id) $ permutations nodes

          print solution

    _ -> putStrLn "did you forget something? I need an input file!"

findMin :: Ord a => [a] -> a
findMin []       = undefined
findMin (x:[])   = x
findMin (x:y:zs) = if x < y then findMin (x:zs) else findMin (y:zs)