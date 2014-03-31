module Hive.Problem.TSP.Permutation
  ( shuffle
  , combine
  ) where

import Control.Monad     (forM)
import System.Random     (randomRIO)
import Data.Array.IO     (IOArray)
import Data.Array.MArray (readArray, writeArray, newListArray)

import Hive.Problem.Data.Graph (Graph, Path, shorterPath)

-------------------------------------------------------------------------------

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
    j  <- randomRIO (i,n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n' = newListArray (1,n')

combine :: (Ord a, Num a) => Graph a -> [Path] -> Path
combine _    []  = []
combine _ (p:[]) = p
combine g (p:ps) = foldr (shorterPath g) p ps