{-# LANGUAGE TemplateHaskell, ImpredicativeTypes #-}

module Hive.Scheduler.Process
  ( runProcess
  )
  where

import Data.Binary (Binary)
import Control.Distributed.Process.Serializable (Serializable)
import Control.Distributed.Process (Closure, send, expect)
import Hive.Types
import Hive.Process

import qualified Control.Distributed.Process as CH (Process)

-------------------------------------------------------------------------------

runProcess :: Process a b -> Scheduler -> a -> CH.Process b
runProcess (Simple closureGen) scheduler x = do
  send scheduler (Task (closureGen x))
  expect

runProcess (Sequence p0 p1) scheduler x = runProcess p0 scheduler x >>= runProcess p1 scheduler
--runProcess (Parallel p0 p1) x = runProcess p0 x `mappend` runProcess p1 x