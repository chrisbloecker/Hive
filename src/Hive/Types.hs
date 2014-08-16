{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Types
  ( Queen
  , Drone
  , Client
  , Scheduler
  , Timeout (unTimeout)
  , milliseconds, seconds, minutes, hours
  , Host
  , Port
  , Task (..)
  , TaskInfo (..)
  , Problem (..)
  , ProblemType (..)  -- reexporting
  , Instance (..)
  , Solution (..)
  , ClientRequest (..)
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process (Process, ProcessId, Closure, SendPort)

import Hive.Imports.MkBinary
import Data.Binary (Word8)
import Control.Monad (liftM2)

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
type Client    = ProcessId
type Scheduler = ProcessId

data ClientRequest = ClientRequest Client Problem               deriving (Generic, Typeable)

data TaskInfo = TaskInfo JobId StepId                           deriving (Generic, Typeable)
type JobId    = Integer
type StepId   = Integer

data Task = Task (Closure (Process ())) deriving (Generic, Typeable)
--data Task a = Task (SendPort a) (Closure a) deriving (Generic, Typeable)
--data Task a where
--  Task :: (Serializable a) => (SendPort a) -> (Closure a) -> Task a

--instance Generic  (Task a) where
--instance Typeable (Task a) where

--instance (Serializable a) => Binary (Task a) where
--  put (Task sPort closure) = put (0 :: Word8) >> put sPort >> put closure
--  get = getWord8 >> liftM2 Task get get

-------------------------------------------------------------------------------

$(derive makeBinary ''ClientRequest)
$(derive makeBinary ''TaskInfo)
$(derive makeBinary ''Task)

-------------------------------------------------------------------------------

milliseconds :: Int -> Timeout
milliseconds ms = Timeout (1000 * ms)

seconds :: Int -> Timeout
seconds s = milliseconds (1000 * s)

minutes :: Int -> Timeout
minutes m = seconds (60 * m)

hours :: Int -> Timeout
hours h = minutes (60 * h)