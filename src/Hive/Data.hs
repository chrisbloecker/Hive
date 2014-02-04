{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Data
  where

import Control.Distributed.Process

import Data.Binary
import Data.Typeable
import Data.DeriveTH
import GHC.Generics (Generic)

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

-- messages from drones
data DRegisterAtQ   = DRegisterAtQ Drone                       deriving (Generic, Typeable, Show)
data DWorkRequestS  = DWorkRequestS Drone                      deriving (Generic, Typeable, Show)
data DWorkFinishedS = DWorkFinishedS Solution Client           deriving (Generic, Typeable, Show)

-- messages from clients
data CSolveProblemQ = CSolveProblemQ ClientRequest             deriving (Generic, Typeable, Show)

-- messages from scheduler
data SSolutionC     = SSolutionC Solution                      deriving (Generic, Typeable, Show)
data SWorkReplyD    = SWorkReplyD (Problem, Client)            deriving (Generic, Typeable, Show)

-- messages from queen
data QRegisteredD   = QRegisteredD Scheduler Logger            deriving (Generic, Typeable, Show)
data QWorkD         = QWorkD Problem                           deriving (Generic, Typeable, Show)
data QEnqueProblemS = QEnqueProblemS ClientRequest             deriving (Generic, Typeable, Show)


$(derive makeBinary ''ProblemType)
$(derive makeBinary ''ClientRequest)

$(derive makeBinary ''DRegisterAtQ)
$(derive makeBinary ''DWorkRequestS)
$(derive makeBinary ''DWorkFinishedS)
$(derive makeBinary ''CSolveProblemQ)
$(derive makeBinary ''SSolutionC)
$(derive makeBinary ''SWorkReplyD)
$(derive makeBinary ''QRegisteredD)
$(derive makeBinary ''QWorkD)
$(derive makeBinary ''QEnqueProblemS)

data DroneState     = DroneState     Queen Scheduler Logger                                      deriving (Show)