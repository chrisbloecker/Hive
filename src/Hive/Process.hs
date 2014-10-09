{-# LANGUAGE GADTs, TemplateHaskell #-}

-- | The core module of Hive.
--
-- In this module we define the constructors and combinators for the Hive process algebra and also the interpreter.
--
-- For example usage check out the "Hive.Problem.Arithmetic" module.
module Hive.Process
  ( Process (..)
  , runProcess
  , mkConst
  , mkSimple
  , mkLocal
  , mkChoice
  , mkSequence
  , mkParallel
  , mkMultilel
  , mkLoop
  , mkSimpleLoop
  ) where

-------------------------------------------------------------------------------

import Hive.Types            (Master)
import Hive.Master.Messaging (getNode, returnNode, getFakeMaster, terminateMaster)

import Control.Monad           (forM)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

import Control.Distributed.Process              (liftIO, getSelfPid, call, spawnLocal)
import Control.Distributed.Process.Serializable (Serializable, SerializableDict)

import qualified Control.Distributed.Process as CH (Process, Closure, Static)

-------------------------------------------------------------------------------

-- | The process algebra
data Process a b where
  -- A constant process that returns a constant value
  Const    :: (Serializable b) => CH.Static (SerializableDict b) -> CH.Closure (CH.Process b) -> Process a b
  -- A simple process that wraps a pure function
  Simple   :: (Serializable b) => CH.Static (SerializableDict b) -> (a -> CH.Closure (CH.Process b)) -> Process a b
  -- A wrapper for a process that will be run locally
  Local    :: (Serializable b) => Process a b -> Process a b
  -- A choice between two processes, based on the input value and another value that are combined into one value.
  -- This combinator inspects the input value and therefore cannot be an arrow.
  Choice   :: (Serializable b) => c -> (a -> c -> c) -> (c -> Bool) -> Process a b -> Process a b -> Process a b
  -- Execute two processes sequentially
  Sequence :: (Serializable b, Serializable c) => Process a c -> Process c b -> Process a b
  -- Execute two processes in parallel and combine the results
  Parallel :: (Serializable b, Serializable c, Serializable d) => Process a c -> Process a d -> Process (c, d) b -> Process a b
  -- Execute a list of processes in parallel and fold the results together
  Multilel :: (Serializable b, Serializable c) => [Process a c] -> b -> Process (b, [c]) b -> Process a b
  -- Repeat the execution of a process, like in a loop, as long as a given predicate holds.
  -- Unlike in imperative programming, we need a value that will be returned in case the predicate doesn't hold. In imperative programming this is represented implicitly by a (global) state.
  -- This combinator inspects the input value and therefore cannot be an arrow.
  Loop     :: (Serializable b) => b -> c -> (c -> Bool) -> (b -> a) -> (a -> c -> c) -> Process a b -> Process a b

-------------------------------------------------------------------------------

-- | Exported function to create a constant process
mkConst :: (Serializable b) => CH.Static (SerializableDict b) -> CH.Closure (CH.Process b) -> Process a b
mkConst = Const

-- | Exported function to create a simple process
mkSimple :: (Serializable b) => CH.Static (SerializableDict b) -> (a -> CH.Closure (CH.Process b)) -> Process a b
mkSimple = Simple

-- | Exported function to wrap a process that will be run locally
mkLocal :: (Serializable b) => Process a b -> Process a b
mkLocal = Local

-- | Exported function to create a choice between two processes
mkChoice :: (Serializable b) => c -> (a -> c -> c) -> (c -> Bool) -> Process a b -> Process a b -> Process a b
mkChoice = Choice

-- | Exported function to run two processes sequentially
mkSequence :: (Serializable b, Serializable c) => Process a c -> Process c b -> Process a b
mkSequence = Sequence

-- | Exported function to run two processes in parallel
mkParallel :: (Serializable b, Serializable c, Serializable d) => Process a c -> Process a d -> Process (c, d) b -> Process a b
mkParallel = Parallel

-- | Exported function to run a list of processes in parallel and fold their outputs together
mkMultilel :: (Serializable b, Serializable c) => [Process a c] -> b -> Process (b, [c]) b -> Process a b
mkMultilel = Multilel

-- | Exported function to create a loop over a process
mkLoop :: (Serializable b) => b -> c -> (c -> Bool) -> (b -> a) -> (a -> c -> c) -> Process a b -> Process a b
mkLoop = Loop

-- | Exported function to create a simple loop
mkSimpleLoop :: (Serializable b) => b -> (a -> Bool) -> (b -> a) -> Process a b -> Process a b
mkSimpleLoop ib pr ba p = Loop ib undefined pr ba const p

-------------------------------------------------------------------------------
-- interpretation of Process structure
-------------------------------------------------------------------------------

-- | This is the interpreter for processes created using the Hive process algebra.
-- To run a process, we need a master node that can give us worker nodes to run the basic processes on.
-- Then we need a process that should be run as well as an input value to run the process on.
runProcess :: Master -> Process a b -> a -> CH.Process b
runProcess master (Const sDict closure) x =
  runProcess master (Simple sDict (const closure)) x

runProcess master (Simple sDict closureGen) x = do
  node <- getNode master =<< getSelfPid
  res  <- call sDict node (closureGen x)
  returnNode master node
  return res

runProcess master (Local p) x = do
  fakeMaster <- getFakeMaster =<< getSelfPid
  res <- runProcess fakeMaster p x
  terminateMaster fakeMaster
  return res

runProcess master (Choice c acc p p1 p2) x =
  runProcess master (if p (acc x c) then p1 else p2) x

runProcess master (Sequence p1 p2) x =
  runProcess master p1 x >>= runProcess master p2

runProcess master (Parallel p1 p2 combinator) x = do
  mvar <- liftIO newEmptyMVar
  spawnLocal $ runProcessHelper master p1 x mvar
  r2 <- runProcess master p2 x
  r1 <- liftIO $ takeMVar mvar
  runProcess master combinator (r1, r2)

runProcess master (Multilel ps ib fold) x = do
  mvars <- forM ps $ \_ -> liftIO newEmptyMVar
  mapM_ (\(proc,mvar) -> spawnLocal $ runProcessHelper master proc x mvar) (ps `zip` mvars)
  ress  <- forM mvars $ \m -> (liftIO . takeMVar $ m)
  runProcess master fold (ib, ress)

runProcess master (Loop ib ic pr ba acc p) x =
  if pr (acc x ic) then do
    x' <- runProcess master p x
    runProcess master (Loop x' (acc x ic) pr ba acc p) (ba x')
  else
    return ib

-------------------------------------------------------------------------------

-- | A helper function to run processes in background.
-- The process helper will be spawned in a new local process and put its result into an MVar.
runProcessHelper :: Master -> Process a b -> a -> MVar b -> CH.Process ()
runProcessHelper master p x mvar = do
  r <- runProcess master p x
  liftIO $ putMVar mvar r

-- | Split lists into chunks of a given size.
split :: Int -> [a] -> [[a]]
split _ [] = []
split i  l = take i l : split i (drop i l)