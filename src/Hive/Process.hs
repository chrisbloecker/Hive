{-# LANGUAGE GADTs #-}

-- | The core module of Hive.
--
-- In this module we define the constructors and combinators for the Hive process algebra and also the interpreter.
--
-- For example usage check out the "Hive.Problem.Arithmetic" module.
module Hive.Process
  ( Process (..)
  , BasicProcess
  , runProcess
  ) where

-------------------------------------------------------------------------------

import Prelude hiding ((>>), (||))

import Hive.Types            (Master)
import Hive.Imports.MkBinary
import Hive.Master.Messaging (getNode, returnNode, getFakeMaster, terminateMaster)

import Data.Monoid

import Control.Arrow           ((&&&))
import Control.Monad           (forM)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

import Control.Distributed.Process              (liftIO, getSelfPid, call, spawnLocal, say)
import Control.Distributed.Process.Serializable (Serializable, SerializableDict)

import qualified Control.Distributed.Process as CH (Process, Closure, Static)

-------------------------------------------------------------------------------

type Predicate a = a -> Bool

-- | A BasicProcess is carried out in the Cloud Haskell Process monad
type BasicProcess b = CH.Process b

-- | The process algebra
data Process a b where
  -- A constant process that returns a constant value
  Const    :: (Serializable b) => CH.Static (SerializableDict b) -> CH.Closure (BasicProcess b) -> Process a b
  -- A simple process that wraps a pure function
  Simple   :: (Serializable b) => CH.Static (SerializableDict b) -> (a -> CH.Closure (BasicProcess b)) -> Process a b
  -- A wrapper for a process that will be run locally
  Local    :: Process a b -> Process a b
  -- A choice between two processes, based on the input value and another value that are combined into one value.
  -- This combinator inspects the input value and therefore cannot be an arrow.
  Choice   :: c -> (a -> c -> d) -> Predicate d -> Process a b -> Process a b -> Process a b
  -- Execute two processes sequentially
  Sequence :: Process a c -> Process c b -> Process a b
  -- Executes one process multiple times and folds the results together
  --Multiple :: (Serializable b) => Process a c -> Int -> b -> Process (b, [c]) b -> Process a b
  -- Execute two processes in parallel and combine the results
  Parallel :: Process a c -> Process a d -> Process (c, d) b -> Process a b
  -- Execute a list of processes in parallel and fold the results together
  Multilel :: [Process a c] -> b -> Process (b, [c]) b -> Process a b
  -- Repeat the execution of a process, like in a loop, as long as a given predicate holds.
  -- Unlike in imperative programming, we need a value that will be returned in case the predicate doesn't hold. In imperative programming this is represented implicitly by a (global) state.
  -- This combinator inspects the input value and therefore cannot be an arrow.
  Loop     :: b -> c -> Predicate c -> (b -> a) -> (a -> c -> c) -> Process a b -> Process a b
  -- Maybe we want to split the data, run different actions on them and combine the results?
  Split    :: (a -> (a,a)) -> Process a c -> Process a d -> Process (c, d) b -> Process a b
  -- Or maybe we want to split the data into many pieces?
  Slice    :: (a -> [a]) -> [Process a c] -> b -> Process (b, [c]) b -> Process a b

instance Show (Process a b) where
  show (Const  _ _            ) = "Const _"
  show (Simple _ _            ) = "Simple _"
  show (Local          p      )  = "Local (" ++ show p ++ ")"
  show (Choice _ _ _   p q    ) = "Choice (" ++ show p ++ " v " ++ show q ++ ")"
  show (Sequence       p q    ) = "Sequence (" ++ show p ++ " > " ++ show q ++ ")"
  --show (Multiple       p i _ r) = "Multiple (" ++ show p ++ " *" ++ show i ++ " -> " ++ show r ++ ")"
  show (Parallel       p q   r) = "Parallel (" ++ show p ++ " | " ++ show q ++ " -> " ++ show r ++ ")"
  show (Multilel       ps _  r) = "Multilel (" ++ concatMap show ps ++ " => " ++ show r ++ ")"
  show (Loop _ _ _ _ _ p      ) = "Loop (" ++ show p ++ ")"

-------------------------------------------------------------------------------
-- interpretation of Process structure
-------------------------------------------------------------------------------

-- | This is the interpreter for processes created using the Hive process algebra.
-- To run a process, we need a master node that can give us worker nodes to run the basic processes on.
-- Then we need a process that should be run as well as an input value to run the process on.
runProcess :: Master -> Process a b -> a -> BasicProcess b
runProcess master (Const sDict closure) x =
  runProcess master (Simple sDict (const closure)) x

runProcess master (Simple sDict closureGen) x = do
  node <- getNode master =<< getSelfPid
  res  <- call sDict node (closureGen x)
  returnNode master node
  return res

runProcess _ (Local p) x = do
  fakeMaster <- getFakeMaster =<< getSelfPid
  res <- runProcess fakeMaster p x
  terminateMaster fakeMaster
  return res

runProcess master (Choice c acd p p1 p2) x =
  runProcess master (if p (acd x c) then p1 else p2) x

runProcess master (Sequence p1 p2) x =
  runProcess master p1 x >>= runProcess master p2

{--
runProcess master (Parallel p1 p2 combinator) x = do
  mvar <- liftIO newEmptyMVar
  _ <- spawnLocal $ runProcessHelper master p1 x mvar
  r2 <- runProcess master p2 x
  r1 <- liftIO $ takeMVar mvar
  runProcess master combinator (r1, r2)
--}
{-- or alternatively: --}
runProcess master (Parallel p1 p2 combinator) x = do
  runProcess master (Split (id &&& id) p1 p2 combinator) x

{--
runProcess master (Multilel ps ib fold) x = do
  mvars <- forM ps $ \_ -> liftIO newEmptyMVar
  mapM_ (\(proc, mvar) -> spawnLocal $ runProcessHelper master proc x mvar) (ps `zip` mvars)
  ress  <- forM mvars $ \mvar -> liftIO . takeMVar $ mvar
  runProcess master fold (ib, ress)
--}
{-- or alternatively: --}
runProcess master (Multilel ps ib fold) x =
  runProcess master (Slice repeat ps ib fold) x

runProcess master (Loop ib ic pr ba acc p) x = do
  if pr (acc x ic) then do
    x' <- runProcess master p x
    runProcess master (Loop x' (acc x ic) pr ba acc p) (ba x')
  else
    return ib

runProcess master (Split split p1 p2 combinator) x = do
  let (xl, xr) = split x
  mvar <- liftIO newEmptyMVar
  _ <- spawnLocal $ runProcessHelper master p1 xl mvar
  r2 <- runProcess master p2 xr
  r1 <- liftIO $ takeMVar mvar
  runProcess master combinator (r1, r2)

runProcess master (Slice slice ps ib fold) x = do
  let pairs = ps `zip` slice x
  mvars <- forM pairs $ \_ -> liftIO newEmptyMVar
  mapM_ (\((proc, x'), mvar) -> spawnLocal $ runProcessHelper master proc x' mvar) (pairs `zip` mvars)
  ress  <- forM mvars $ \mvar -> liftIO . takeMVar $ mvar
  runProcess master fold (ib, ress)

-------------------------------------------------------------------------------

-- | A helper function to run processes in background.
-- The process helper will be spawned in a new local process and put its result into an MVar.
runProcessHelper :: Master -> Process a b -> a -> MVar b -> CH.Process ()
runProcessHelper master p x mvar = do
  r <- runProcess master p x
  liftIO $ putMVar mvar r