module Hive.Problem
  ( handle
  ) where

import Hive.Interface

import Data.Text.Lazy (pack, unpack)

import qualified Control.Distributed.Process as CH
import qualified Hive.Problem.Arithmetic as Arithmetic

-------------------------------------------------------------------------------

handle :: Problem -> Master -> CH.Process Solution
handle (Problem ARITH inst) master = do
  let expr = Arithmetic.parse . unpack . unInstance $ inst
  let proc = Arithmetic.interpret expr
  solution <- runProcess master proc 0
  return . Solution . pack . show $ solution

handle (Problem _ _) _master = return NotImplemented