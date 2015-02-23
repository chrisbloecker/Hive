{-# LANGUAGE GADTs #-}

-- | The core module of Hive.
--
-- In this module we define the constructors and combinators for the Hive process algebra and also the interpreter.
--
-- For example usage check out the "Hive.Problem.Arithmetic" module.
module Process
  ( Process (..)
  , BasicProcess
  , Predicate
  , runProcess
  ) where

-------------------------------------------------------------------------------

import Control.Monad           (forM)
import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

-------------------------------------------------------------------------------

type BasicProcess a b = a -> IO b
type Predicate a = Process a Bool

-- | The process algebra
data Process a b where
  -- The identity process
  Id         :: Process a a
  -- The error process
  Err        :: Process a a
  -- A simple process that wraps a pure function
  Basic      :: BasicProcess a b -> Process a b
  -- A choice between two processes, based on the input value and another value that are combined into one value.
  -- This combinator inspects the input value and therefore cannot be an arrow.
  Choice     :: Predicate a -> Process a b -> Process a b -> Process a b
  -- Execute two processes sequentially
  Sequence   :: Process a c -> Process c b -> Process a b
  -- Execute two processes in parallel and combine the results
  Parallel   :: Process (c, d) b -> Process a c -> Process a d -> Process a b
  -- Execute a list of processes in parallel and fold the results together
  Multilel   :: [Process a c] -> Process (a, [c]) b -> Process a b
  -- Repeat the execution of a process, like in a loop, as long as a given predicate holds.
  -- This combinator inspects the input value and therefore cannot be an arrow.
  Repetition :: Predicate a -> Process a a -> Process a a

-------------------------------------------------------------------------------
-- interpretation of Process structure
-------------------------------------------------------------------------------

-- | This is the interpreter for processes created using the Hive process algebra.
-- To run a process, we need a master node that can give us worker nodes to run the basic processes on.
-- Then we need a process that should be run as well as an input value to run the process on.
runProcess :: Process a b -> a -> IO b
runProcess Id x =
  return x

runProcess Err _ =
  return undefined

runProcess (Basic f) x =
  f x

runProcess (Choice pr p1 p2) x = do
  b <- runProcess pr x
  runProcess (if b then p1 else p2) x

runProcess (Sequence p1 p2) x =
  runProcess p1 x >>= \y -> runProcess p2 y

runProcess (Parallel combinator p1 p2) x = do
  mvar <- newEmptyMVar
  _ <- forkIO $ runProcessHelper p1 x mvar
  r2 <- runProcess p2 x
  r1 <- takeMVar mvar
  runProcess combinator (r1, r2)

{--
runProcess (Multilel ps fold) x = do
  mvars <- forM ps $ const newEmptyMVar
  mapM_ (\(comp, mvar) -> forkIO $ runProcessHelper comp x mvar) (ps `zip` mvars)
  rs  <- forM mvars takeMVar
  runProcess fold (x, rs)
--}

runProcess (Multilel ps fold) x =
  runProcess (foldr (Parallel consP) emptyListP ps `Sequence` pairP x `Sequence` fold) x
    where
      pairP :: a -> Process b (a, b)
      pairP x = Basic (return . (,) x)

      consP :: Process (c, [c]) [c]
      consP = Basic (return . uncurry (:))

      emptyListP :: Process a [c]
      emptyListP = Basic (return . const [])

runProcess rep@(Repetition pr p) x =
  runProcess (Choice pr (p `Sequence` rep) Id) x

-------------------------------------------------------------------------------

-- | A helper function to run processes in background.
-- The process helper will be spawned in a new local process and put its result into an MVar.
runProcessHelper :: Process a b -> a -> MVar b -> IO ()
runProcessHelper p x mvar = putMVar mvar =<< runProcess p x

-------------------------------------------------------------------------------