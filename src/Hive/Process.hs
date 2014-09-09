{-# LANGUAGE GADTs, TemplateHaskell #-}

module Hive.Process
  ( Process (Const, Simple, Choice, Sequence, Parallel)
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

import qualified Control.Distributed.Process      as CH (Process, Closure, Static)

-------------------------------------------------------------------------------

data Process a b where
  Const    :: (Serializable b) => CH.Static (SerializableDict b) -> CH.Closure (CH.Process b) -> Process a b
  Simple   :: (Serializable b) => CH.Static (SerializableDict b) -> (a -> CH.Closure (CH.Process b)) -> Process a b
  Local    :: (Serializable b) => Process a b -> Process a b
  Choice   :: (Serializable b) => (a -> Bool) -> Process a b -> Process a b -> Process a b
  Sequence :: (Serializable b, Serializable c) => Process a c -> Process c b -> Process a b
  Parallel :: (Serializable b) => Process a b -> Process a b -> Process (b, b) b -> Process a b
  Multilel :: (Serializable b, Serializable c) => [Process a c] -> b -> Process (b, [c]) b -> Process a b
  Loop     :: (Serializable b) => b -> c -> (c -> Bool) -> (b -> a) -> (a -> c -> c) -> Process a b -> Process a b

-------------------------------------------------------------------------------

mkConst :: (Serializable b) => CH.Static (SerializableDict b) -> CH.Closure (CH.Process b) -> Process a b
mkConst = Const

mkSimple :: (Serializable b) => CH.Static (SerializableDict b) -> (a -> CH.Closure (CH.Process b)) -> Process a b
mkSimple = Simple

mkLocal :: (Serializable b) => Process a b -> Process a b
mkLocal = Local

mkChoice :: (Serializable b) => (a -> Bool) -> Process a b -> Process a b -> Process a b
mkChoice = Choice

mkSequence :: (Serializable b, Serializable c) => Process a c -> Process c b -> Process a b
mkSequence = Sequence

mkParallel :: (Serializable b) => Process a b -> Process a b -> Process (b, b) b -> Process a b
mkParallel = Parallel

mkMultilel :: (Serializable b, Serializable c) => [Process a c] -> b -> Process (b, [c]) b -> Process a b
mkMultilel = Multilel

mkLoop :: (Serializable b) => b -> c -> (c -> Bool) -> (b -> a) -> (a -> c -> c) -> Process a b -> Process a b
mkLoop = Loop

mkSimpleLoop :: (Serializable b) => b -> (a -> Bool) -> (b -> a) -> Process a b -> Process a b
mkSimpleLoop ib pr ba p = Loop ib undefined pr ba const p

-------------------------------------------------------------------------------
-- interpretation of Process structure
-------------------------------------------------------------------------------

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

runProcess master (Choice p p1 p2) x =
  runProcess master (if p x then p1 else p2) x

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

runProcessHelper :: Master -> Process a b -> a -> MVar b -> CH.Process ()
runProcessHelper master p x mvar = do
  r <- runProcess master p x
  liftIO $ putMVar mvar r

split :: Int -> [a] -> [[a]]
split _ [] = []
split i  l = take i l : split i (drop i l)