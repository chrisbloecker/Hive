{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Types
  where

import Control.Distributed.Process (ProcessId)

import Data.Binary      (Binary, put, get)
import Data.Typeable    (Typeable)
import Data.DeriveTH    (derive, makeBinary)
import GHC.Generics     (Generic)

type QueenSearchReply = Maybe ProcessId

type Queen     = ProcessId
type Drone     = ProcessId
type Client    = ProcessId
type Scheduler = ProcessId
type Logger    = ProcessId


type Problem  = String
type Solution = String

data ProblemType   = TSP                                       deriving (Generic, Typeable, Show)
data ClientRequest = ClientRequest Client ProblemType Problem  deriving (Generic, Typeable, Show)

$(derive makeBinary ''ProblemType)
$(derive makeBinary ''ClientRequest)