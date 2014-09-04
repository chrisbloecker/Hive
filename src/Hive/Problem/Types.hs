{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.Types
  ( ProblemType (..)
  , Problem (Problem, problemType, problemInstance)
  , Solution (..)
  ) where

import Data.Data
import Data.Text.Lazy   (Text)
import Data.Text.Binary ()
import Data.Binary      (Binary, put, putWord8, get, getWord8)
import Data.DeriveTH    (derive, makeBinary)
import Data.Aeson.TH    (deriveJSON, defaultOptions)
import GHC.Generics     (Generic)

-------------------------------------------------------------------------------

data ProblemType  = ARITH -- Arithmetic Expression
                  | TSP   -- Traveling Salesman Problem
                  | TSPL  -- the same with Poslist
                  | SSSP  -- Single Source Shortest Path
  deriving (Eq, Ord, Show, Enum, Bounded, Data, Generic, Typeable)

data Solution = Solution { unSolution :: Text }
              | NoSolution
              | InvalidInput
              | TimeoutReached
              | NotImplemented
  deriving (Eq, Ord, Data, Generic, Typeable, Show)

data Problem = Problem { problemType     :: ProblemType
                       , problemInstance :: Text
                       }
  deriving (Eq, Ord, Data, Generic, Typeable, Show)

-------------------------------------------------------------------------------

-- for sending it over network
$(derive makeBinary ''ProblemType)
$(derive makeBinary ''Problem)
$(derive makeBinary ''Solution)

-- for reading it from JSON
$(deriveJSON defaultOptions ''ProblemType)
$(deriveJSON defaultOptions ''Problem)