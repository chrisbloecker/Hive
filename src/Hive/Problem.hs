module Hive.Problem
  ( handle
  ) where

-------------------------------------------------------------------------------

import Hive.Interface

import Data.Aeson              (decode')
import Data.Text.Lazy          (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)

import qualified Control.Distributed.Process as CH
import qualified Hive.Problem.Arithmetic as Arithmetic

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

handle (Problem _ _) _master = return NotImplemented