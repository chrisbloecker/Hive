{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Types
  ( Queen
  , Drone
  , Warrior
  , Client
  , Scheduler
  , Logger
  , CPUInfo
  , Task (..)
  , Statistics (..)
  , Problem (..)
  , ProblemType (..)  -- reexporting
  , Instance (..)
  , Solution (..)
  , QueenSearchReply  -- ToDo: Why here?
  , ClientRequest (..)
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process (Process, ProcessId, Closure)

import Data.Binary             (Binary, put, get)
import Data.Typeable           (Typeable)
import Data.DeriveTH           (derive, makeBinary)
import GHC.Generics            (Generic)

import Hive.Problem.Types      ( ProblemType (..)
                               , Problem (..)
                               , Instance (..)
                               , Solution (..)
                               )

-------------------------------------------------------------------------------

type Queen     = ProcessId
type Drone     = ProcessId
type Warrior   = ProcessId
type Client    = ProcessId
type Scheduler = ProcessId
type Logger    = ProcessId

type CPUInfo   = String

type QueenSearchReply = Maybe Queen

data ClientRequest = ClientRequest Client Problem      deriving (Generic, Typeable, Show)

data Task          = Task (Closure (Process ()))       deriving (Generic, Typeable, Show)

data Statistics    = Statistics { cpus :: [CPUInfo]
                                }                      deriving (Generic, Typeable, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''ClientRequest)
$(derive makeBinary ''Task)
$(derive makeBinary ''Statistics)