{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.Types
  (ProblemType(..)
  ) where

import Data.Binary
import Data.Typeable (Typeable)
import Data.DeriveTH (derive, makeBinary)
import GHC.Generics  (Generic)

data ProblemType  = TSP  -- Traveling Salesman Problem
                  | SSSP -- Single Source Shortest Path
                  | APSP -- All Pair Shortest Path
  deriving (Generic, Typeable, Show)

$(derive makeBinary ''ProblemType)