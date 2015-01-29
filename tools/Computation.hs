{-# LANGUAGE GADTs #-}

-- | The core module of Hive.
--
-- In this module we define the constructors and combinators for the Hive process algebra and also the interpreter.
--
-- For example usage check out the "Hive.Problem.Arithmetic" module.
module Computation
  ( Computation (..)
  , runComputation
  ) where

-------------------------------------------------------------------------------

import Control.Monad           (forM)
import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

-------------------------------------------------------------------------------

-- | The process algebra
data Computation a b where
  -- A constant process that returns a constant value
  Const    :: IO b -> Computation a b
  -- A simple process that wraps a pure function
  Simple   :: (a -> IO b) -> Computation a b
  -- A choice between two processes, based on the input value and another value that are combined into one value.
  -- This combinator inspects the input value and therefore cannot be an arrow.
  Choice   :: c -> (a -> c -> d) -> (d -> Bool) -> Computation a b -> Computation a b -> Computation a b
  -- Execute two processes sequentially
  Sequence :: Computation a c -> Computation c b -> Computation a b
  -- Executes one process multiple times and folds the results together
  --Multiple :: (Serializable b) => Process a c -> Int -> b -> Process (b, [c]) b -> Process a b
  -- Execute two processes in parallel and combine the results
  Parallel :: Computation a c -> Computation a d -> Computation (c, d) b -> Computation a b
  -- Execute a list of processes in parallel and fold the results together
  Multilel :: [Computation a c] -> b -> Computation (b, [c]) b -> Computation a b
  -- Repeat the execution of a process, like in a loop, as long as a given predicate holds.
  -- Unlike in imperative programming, we need a value that will be returned in case the predicate doesn't hold. In imperative programming this is represented implicitly by a (global) state.
  -- This combinator inspects the input value and therefore cannot be an arrow.
  Loop     :: b -> c -> (c -> Bool) -> (b -> a) -> (a -> c -> c) -> Computation a b -> Computation a b
  -- Maybe we want to split the data, run different actions on them and combine the results?
  Split    :: (a -> (a,a)) -> Computation a c -> Computation a d -> Computation (c, d) b -> Computation a b
  -- Or maybe we want to split the data into many pieces?
  Slice    :: (a -> [a]) -> [Computation a c] -> b -> Computation (b, [c]) b -> Computation a b

-------------------------------------------------------------------------------
-- interpretation of Process structure
-------------------------------------------------------------------------------

-- | This is the interpreter for processes created using the Hive process algebra.
-- To run a process, we need a master node that can give us worker nodes to run the basic processes on.
-- Then we need a process that should be run as well as an input value to run the process on.
runComputation :: Computation a b -> a -> IO b
runComputation (Const c) _ =
  c

runComputation (Simple c) x = do
  c x

runComputation (Choice c acd p c1 c2) x =
  runComputation (if p (acd x c) then c1 else c2) x

runComputation (Sequence c1 c2) x =
  runComputation c1 x >>= runComputation c2

runComputation (Parallel c1 c2 combinator) x = do
  mvar <- newEmptyMVar
  _ <- forkIO $ runComputationHelper c1 x mvar
  r2 <- runComputation c2 x
  r1 <- takeMVar mvar
  runComputation combinator (r1, r2)

runComputation (Multilel cs ib fold) x = do
  mvars <- forM cs $ const newEmptyMVar
  mapM_ (\(comp, mvar) -> forkIO $ runComputationHelper comp x mvar) (cs `zip` mvars)
  ress  <- forM mvars takeMVar
  runComputation fold (ib, ress)

runComputation (Loop ib ic pr ba acc c) x = do
  if pr (acc x ic) then do
    x' <- runComputation c x
    runComputation (Loop x' (acc x ic) pr ba acc c) (ba x')
  else
    return ib

runComputation (Split split c1 c2 combinator) x = do
  let (xl, xr) = split x
  mvar <- newEmptyMVar
  _ <- forkIO $ runComputationHelper c1 xl mvar
  r2 <- runComputation c2 xr
  r1 <- takeMVar mvar
  runComputation combinator (r1, r2)

runComputation (Slice slice cs ib fold) x = do
  let pairs = cs `zip` slice x
  mvars <- forM pairs $ const newEmptyMVar
  mapM_ (\((proc, x'), mvar) -> forkIO $ runComputationHelper proc x' mvar) (pairs `zip` mvars)
  ress  <- forM mvars $ \mvar -> takeMVar mvar
  runComputation fold (ib, ress)

-------------------------------------------------------------------------------

-- | A helper function to run processes in background.
-- The process helper will be spawned in a new local process and put its result into an MVar.
runComputationHelper :: Computation a b -> a -> MVar b -> IO ()
runComputationHelper c x mvar = putMVar mvar =<< runComputation c x