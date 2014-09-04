{-# LANGUAGE GADTs, TemplateHaskell #-}

module Hive.Process
  ( Process (Const, Simple, Choice, Sequence, Parallel)
  , runProcess
  , mkConst
  , mkSimple
  , mkChoice
  , mkSequence
  , mkParallel
  , mkMultilel
  , mkLoop
  , mkInit
  , mkAction
  , mkPredicate
  , mkLoopHead
  ) where

-------------------------------------------------------------------------------

import Hive.Types            (Master)
import Hive.Master.Messaging (getNode, returnNode)

import Control.Monad           (forM)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

import Control.Distributed.Process              (liftIO, getSelfPid, call, spawnLocal)
import Control.Distributed.Process.Serializable (Serializable, SerializableDict)

import qualified Control.Distributed.Process      as CH (Process, Closure, Static)

-------------------------------------------------------------------------------

data Init a where
  Init :: a -> Init a

data Action a where
  Action :: (a -> a) -> Action a

data Predicate a where
  Predicate :: (a -> Bool) -> Predicate a

data LoopHead a where
  LoopHead :: Init a -> Predicate a -> Action a -> LoopHead a

data Process a b where
  Const    :: (Serializable b) => CH.Static (SerializableDict b) -> CH.Closure (CH.Process b) -> Process a b
  Simple   :: (Serializable b) => CH.Static (SerializableDict b) -> (a -> CH.Closure (CH.Process b)) -> Process a b
  Choice   :: (Serializable b) => (a -> Bool) -> Process a b -> Process a b -> Process a b
  Sequence :: (Serializable b, Serializable c) => Process a c -> Process c b -> Process a b
  Parallel :: (Serializable b) => Process a b -> Process a b -> Process (b, b) b -> Process a b
  Multilel :: (Serializable b) => [Process a b] -> Process (b, b) b -> Process a b
  Loop     :: (Serializable b) => LoopHead b -> Process b b -> Process b b

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

mkMultilel :: (Serializable b) => [Process a b] -> Process (b, b) b -> Process a b
mkMultilel = Multilel

mkLoop :: (Serializable b) => LoopHead b -> Process b b -> Process b b
mkLoop = Loop

mkInit :: a -> Init a
mkInit = Init

mkAction :: (a -> a) -> Action a
mkAction = Action

mkPredicate :: (a -> Bool) -> Predicate a
mkPredicate = Predicate

mkLoopHead :: Init a -> Predicate a -> Action a -> LoopHead a
mkLoopHead = LoopHead

-------------------------------------------------------------------------------
-- interpretation of Process structure
-------------------------------------------------------------------------------

runProcess :: Master -> Process a b -> a -> CH.Process b
runProcess master (Const sDict closure) _ = do
  node <- getNode master =<< getSelfPid
  res  <- call sDict node closure
  returnNode master node
  return res

runProcess master (Simple sDict closureGen) x = do
  node <- getNode master =<< getSelfPid
  res  <- call sDict node (closureGen x)
  returnNode master node
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

runProcess master (Multilel ps combinator) x = do
  mvars <- forM ps $ \_ -> liftIO newEmptyMVar
  mapM_ (\(proc,mvar) -> spawnLocal $ runProcessHelper master proc x mvar) (ps `zip` mvars)
  ress  <- forM mvars $ \m -> (liftIO . takeMVar $ m)
  combine master ress combinator

runProcess master (Loop (LoopHead (Init i) (Predicate pr) (Action a)) p) x =
  if pr i then do
    runProcess master (Loop (LoopHead (Init (a i)) (Predicate pr) (Action a)) p) =<< runProcess master p x
  else
    return x

-------------------------------------------------------------------------------

runProcessHelper :: Master -> Process a b -> a -> MVar b -> CH.Process ()
runProcessHelper master p x mvar = do
  r <- runProcess master p x
  liftIO $ putMVar mvar r

combine :: Master -> [a] -> Process (a,a) a -> CH.Process a
combine _ (r:[]) p = return r

combine master rs p = do
  let pairs = toPairs rs
  mvars <- forM pairs $ \_ -> liftIO newEmptyMVar
  mapM_ (\(pair, mvar) -> spawnLocal $ runProcessHelper master p pair mvar) (pairs `zip` mvars)
  ress <- forM mvars $ \m -> (liftIO . takeMVar $ m)
  combine master ress p

toPairs       [] = []
toPairs (x:y:es) = (x,y):toPairs es
toPairs (  x:[]) = [(x,x)]
