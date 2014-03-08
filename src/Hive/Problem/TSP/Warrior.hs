{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Problem.TSP.Warrior
  where

import Control.Distributed.Process (Process, ProcessId, send, getSelfPid)
import Control.Distributed.Process.Closure (remotable, mkClosure)

import Data.Binary   (Binary, put, get)
import Data.Typeable (Typeable)
import Data.DeriveTH (derive, makeBinary)
import GHC.Generics  (Generic)

import Hive.Types
import Hive.Messages (WTaskS (..))
import Hive.Problem.Data.External.Graph (Graph)

-------------------------------------------------------------------------------

type Worker    = ProcessId
data Register  = Register Worker   deriving (Generic, Typeable, Show)

-------------------------------------------------------------------------------

$(derive makeBinary ''Register)

-------------------------------------------------------------------------------

worker :: Warrior -> Process ()
worker warrior = do
  self <- getSelfPid
  send warrior $ Register self

remotable ['worker]

-------------------------------------------------------------------------------

run :: Scheduler -> Client -> Maybe Graph -> Process ()
run scheduler client _graph = do
  self <- getSelfPid
  let task = Task ($(mkClosure 'worker) (self :: Warrior))
  send scheduler $ WTaskS self task