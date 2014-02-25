{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Types
  ( Queen
  , Drone
  , Client
  , Scheduler
  , Logger
  , Problem(Problem)
  , ProblemType(..)   -- reexporting
  , Instance(Instance, unInstance)
  , Solution(Solution)
  , QueenSearchReply  -- ToDo: Why here?
  , ClientRequest(ClientRequest)
  ) where

-------------------------------------------------------------------------------

-- For ProcessId
import Control.Distributed.Process (ProcessId)

-- For the magic
import Data.Binary        (Binary, put, get)
import Data.Typeable      (Typeable)
import Data.DeriveTH      (derive, makeBinary)
import GHC.Generics       (Generic)

-- For problem instances
import Data.Text          (Text)
import Data.Text.Binary   ()

-- For ProblemType
import Hive.Problem.Types (ProblemType(..))

-------------------------------------------------------------------------------

type Queen     = ProcessId
type Drone     = ProcessId
type Client    = ProcessId
type Scheduler = ProcessId
type Logger    = ProcessId

type QueenSearchReply = Maybe Queen

newtype Instance = Instance { unInstance :: Text } deriving (Generic, Typeable, Show)

data Problem  = Problem ProblemType Instance       deriving (Generic, Typeable, Show)
data Solution = Solution Text                      deriving (Generic, Typeable, Show)

data ClientRequest = ClientRequest Client Problem  deriving (Generic, Typeable, Show)

-------------------------------------------------------------------------------

-- Let the magic begin...
$(derive makeBinary ''Instance)
$(derive makeBinary ''Problem)
$(derive makeBinary ''Solution)
$(derive makeBinary ''ClientRequest)