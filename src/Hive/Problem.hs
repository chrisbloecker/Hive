module Hive.Problem
  ( handle
  ) where

-------------------------------------------------------------------------------

import Hive.Interface
import Hive.Data.Graph
import Hive.Data.Poslist
import Hive.Problem.TSP.Pheromones

import Data.Aeson              (decode', encode)
import Data.Text.Lazy          (pack)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import qualified Control.Distributed.Process as CH
import qualified Hive.Problem.Arithmetic as Arithmetic
import qualified Hive.Problem.TSP        as TSP

-------------------------------------------------------------------------------

handle :: Problem -> Master -> CH.Process Solution
handle (Problem ARITH inst) master = do
  let mExpr = decode' . encodeUtf8 $ inst :: Maybe Arithmetic.Expr
  case mExpr of
    Nothing   -> return InvalidInput
    Just expr -> do
      let proc = Arithmetic.interpret expr
      solution <- runProcess master proc 0
      return . Solution . pack . show $ solution

handle (Problem TSP inst) master = do
  let mGraph = Hive.Data.Graph.parse inst
  case mGraph of
    Nothing -> return InvalidInput
    Just graph -> do
      let configuration = TSP.mkConfiguration graph 10 (size graph) 3 5
      let proc = TSP.interpret configuration
      solution <- runProcess master proc (configuration, mkPheromones graph 1.0)
      return . Solution . pack . show $ solution

handle (Problem TSPL inst) master = do
  let mPoslist = Hive.Data.Poslist.parse inst
  case mPoslist of
    Nothing -> return InvalidInput
    Just poslist -> do
      let graph = convertToGraph poslist
      handle (Problem TSP (decodeUtf8 . encode $ graph)) master

handle (Problem _ _) _master = return NotImplemented