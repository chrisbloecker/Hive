{-# LANGUAGE TemplateHaskell #-}

module Hive.Scheduler.Process
  ( runProcess
  , __remoteTable
  )
  where

import Control.Distributed.Process.Closure (remotable, mkClosure)
import Control.Distributed.Process.Serializable (Serializable)
import Data.Typeable (Typeable)
import Control.Distributed.Process (Closure, SendPort, unClosure, newChan, send, sendChan, receiveChan)
import Hive.Types
import Hive.Process

import qualified Control.Distributed.Process as CH (Process)

-------------------------------------------------------------------------------

wrapClosure :: (Serializable a) => (Closure a, SendPort a) -> CH.Process a
wrapClosure (closure, sendPort) = do
  res <- unClosure closure
  sendChan sendPort res
  return ()

-------------------------------------------------------------------------------

remotable ['wrapClosure]

-------------------------------------------------------------------------------

runProcess :: Process a b -> Scheduler -> a -> CH.Process b
runProcess (Simple closure) scheduler x = do
  (sPort, rPort) <- newChan
  let clo = closure x
  let task = Task ($(mkClosure 'wrapClosure) (clo, sPort))
  send scheduler task
  receiveChan rPort

runProcess (Sequence p0 p1) scheduler x = runProcess p0 scheduler x >>= runProcess p1 scheduler
--runProcess (Parallel p0 p1) x = runProcess p0 x `mappend` runProcess p1 x