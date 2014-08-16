{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.Types
  ( ProblemType (..)
  , Problem (Problem, problemType, inst)
  , Instance (Instance, unInstance)
  , Solution (..)
  ) where

import Data.Text.Lazy   (Text)
import Data.Text.Binary ()
import Data.Binary      (Binary, put, putWord8, get, getWord8)
import Data.Typeable    (Typeable)
import Data.DeriveTH    (derive, makeBinary)
import Data.Aeson.TH    (deriveJSON, defaultOptions)
import GHC.Generics     (Generic)

-------------------------------------------------------------------------------

data ProblemType  = ARITH -- Arithmetic Expression
                  | TSP   -- Traveling Salesman Problem
                  | SSSP  -- Single Source Shortest Path
                  | APSP  -- All Pair Shortest Path
  deriving (Eq, Show, Enum, Bounded, Generic, Typeable)

newtype Instance = Instance { unInstance :: Text }           deriving (Generic, Typeable, Show)

data Solution = Solution { unSolution :: Text
                         , unValue    :: Int
                         }
              | NoSolution
              | InvalidInput
              | TimeoutReached
              | NotImplemented
  deriving (Generic, Typeable, Show)

data Problem = Problem  { problemType :: ProblemType
                        , inst        :: Instance
                        }
  deriving (Generic, Typeable, Show)

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