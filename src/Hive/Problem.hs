module Hive.Problem
  ( handle
  ) where

-------------------------------------------------------------------------------

import Hive.Interface
import Hive.Data.Graph
import Hive.Data.Poslist
import Hive.Problem.TSP.Pheromones

import Data.Aeson              (decode', encode)
import Data.Text.Lazy          (Text, pack)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import qualified Control.Distributed.Process as CH

import qualified Hive.Problem.Arithmetic as Arithmetic
import qualified Hive.Problem.TSP        as TSP

-------------------------------------------------------------------------------

handle :: Problem -> Master -> CH.Process Solution
handle (Problem ARITH inst) master =
  maybe (return InvalidInput)
        (\expr -> return . Solution . pack . show =<< runProcess master (Arithmetic.interpret expr) ())
        (Arithmetic.parse inst)

handle (Problem TSP inst) master =
  case Hive.Data.Graph.parse inst of
    Nothing -> return InvalidInput
    Just graph -> do
      let configuration = TSP.Configuration graph (mkPheromones graph 2) (nodes graph) (pathLength' graph (nodes graph)) (size graph) 100 2 5 0.1
      let proc = TSP.interpret configuration
      return . Solution . pack . show =<< runProcess master proc configuration

handle (Problem TSPL inst) master = 
  maybe (return InvalidInput)
        (\poslist -> handle (Problem TSP (decodeUtf8 . encode . convertToGraph $ poslist)) master)
        (Hive.Data.Poslist.parse inst)

handle (Problem _ _) _master =
  return NotImplemented
