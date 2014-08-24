{-# LANGUAGE GADTs, TemplateHaskell #-}

module Hive.Process
  ( Process (Const, Simple, Choice, Sequence, Parallel)
  , runProcess
  , mkConst
  , mkSimple
  , mkChoice
  , mkSequence
  , mkParallel
  ) where

-------------------------------------------------------------------------------

import Hive.Types            (Master)
import Hive.Master.Messaging (getNode, returnNode)

import Control.Distributed.Process              (getSelfPid, call)
import Control.Distributed.Process.Serializable (Serializable, SerializableDict)

import qualified Control.Distributed.Process as CH (Process, Closure, Static)

-------------------------------------------------------------------------------

data Process a b where
  Const    :: (Serializable b) => CH.Static (SerializableDict b) -> CH.Closure (CH.Process b) -> Process a b
  Simple   :: (Serializable b) => CH.Static (SerializableDict b) -> (a -> CH.Closure (CH.Process b)) -> Process a b
  Choice   :: (Serializable b) => (a -> Bool) -> Process a b -> Process a b -> Process a b
  Sequence :: (Serializable b, Serializable c) => Process a c -> Process c b -> Process a b
  Parallel :: (Serializable b) => Process a b -> Process a b -> Process (b, b) b -> Process a b

-------------------------------------------------------------------------------

mkConst :: (Serializable b) => CH.Static (SerializableDict b) -> CH.Closure (CH.Process b) -> Process a b
mkConst = Const

mkSimple :: (Serializable b) => CH.Static (SerializableDict b) -> (a -> CH.Closure (CH.Process b)) -> Process a b
mkSimple = Simple

mkChoice :: (Serializable b) => (a -> Bool) -> Process a b -> Process a b -> Process a b
mkChoice = Choice

mkSequence :: (Serializable b, Serializable c) => Process a c -> Process c b -> Process a b
mkSequence = Sequence

mkParallel :: (Serializable b) => Process a b -> Process a b -> Process (b, b) b -> Process a b
mkParallel = Parallel

-------------------------------------------------------------------------------
-- interpretation of Process structure
-------------------------------------------------------------------------------

runProcess :: Master -> Process a b -> a -> CH.Process b
runProcess master (Const sDict closure) _ = do
  node <- getNode master =<< getSelfPid
  res <- call sDict node closure
  returnNode master node
  return res

runProcess master (Simple sDict closureGen) x = do
  node <- getNode master =<< getSelfPid
  res <- call sDict node (closureGen x)
  returnNode master node
  return res

runProcess master (Choice p p1 p2) x =
  runProcess master (if p x then p1 else p2) x

runProcess master (Sequence p1 p2) x = do
  runProcess master p1 x >>= runProcess master p2

runProcess master (Parallel p1 p2 combinator) x = do
  r1 <- runProcess master p1 x
  r2 <- runProcess master p2 x
  runProcess master combinator (r1, r2)