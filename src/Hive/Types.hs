{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Types
  ( Queen
  , Drone
  , Warrior
  , Client
  , Scheduler
  , Logger
  , Timeout (unTimeout)
  , milliseconds, seconds, minutes, hours
  , Host
  , Port
  , CPUInfo
  , Task (..)
  , Statistics (..)
  , Problem (..)
  , ProblemType (..)  -- reexporting
  , Instance (..)
  , Solution (..)
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

newtype Timeout = Timeout { unTimeout :: Int }  deriving (Eq, Show)

type Host = String
type Port = String

type Queen     = ProcessId
type Drone     = ProcessId
type Warrior   = ProcessId
type Client    = ProcessId
type Scheduler = ProcessId
type Logger    = ProcessId

type CPUInfo   = String

data ClientRequest = ClientRequest Client Problem      deriving (Generic, Typeable, Show)
data Task          = Task (Closure (Process ()))       deriving (Generic, Typeable, Show)

data Statistics    = Statistics { cpus :: [CPUInfo]
                                }                      deriving (Generic, Typeable, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''ClientRequest)
$(derive makeBinary ''Task)
$(derive makeBinary ''Statistics)

-------------------------------------------------------------------------------

milliseconds :: Int -> Timeout
milliseconds ms = Timeout (1000 * ms)

seconds :: Int -> Timeout
seconds s = milliseconds (1000 * s)

minutes :: Int -> Timeout
minutes m = seconds (60 * m)

hours :: Int -> Timeout
hours h = minutes (60 * h)