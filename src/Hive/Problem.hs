module Hive.Problem
  ( handle
  ) where

-------------------------------------------------------------------------------

import Hive.Interface
import Hive.Data.Graph
import Hive.Problem.TSP.Pheromones

import Data.Aeson              (decode')
import Data.Text.Lazy          (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)

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
  let mGraph = decode' . encodeUtf8 $ inst :: Maybe (Graph Int)
  case mGraph of
    Nothing -> return InvalidInput
    Just graph -> do
      CH.say "I can read this graph!"
      let configuration = TSP.mkConfiguration graph 10 (size graph) 3 5
      CH.say "And I have a configuration"
      let proc = TSP.interpret configuration
      CH.say "Process created... let's run it"
      solution <- runProcess master proc (configuration, mkPheromones graph 1.0)
      CH.say $ "There it is... my solution: " ++ show solution
      return . Solution . pack . show $ solution

handle (Problem _ _) _master = return NotImplemented