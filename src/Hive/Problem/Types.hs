{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.Types
  ( ProblemType (..)
  , Problem (Problem, problemType, inst)
  , Instance (Instance, unInstance)
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
                  | SSSP  -- Single Source Shortest Path
  deriving (Eq, Ord, Show, Enum, Bounded, Data, Generic, Typeable)

newtype Instance = Instance { unInstance :: Text } deriving (Eq, Ord, Data, Generic, Typeable, Show)

data Solution = Solution { unSolution :: Text }
              | NoSolution
              | InvalidInput
              | TimeoutReached
              | NotImplemented
  deriving (Eq, Ord, Data, Generic, Typeable, Show)

data Problem = Problem  { problemType :: ProblemType
                        , inst        :: Instance
                        }
  deriving (Eq, Ord, Data, Generic, Typeable, Show)

-------------------------------------------------------------------------------

-- for sending it over network
$(derive makeBinary ''ProblemType)
$(derive makeBinary ''Instance)
$(derive makeBinary ''Problem)
$(derive makeBinary ''Solution)

-- for reading it from JSON
$(deriveJSON defaultOptions ''ProblemType)
$(deriveJSON defaultOptions ''Instance)
$(deriveJSON defaultOptions ''Problem)