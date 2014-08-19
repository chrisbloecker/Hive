{-# LANGUAGE GADTs, TemplateHaskell #-}

module Hive.Process
  ( Process (Simple, Sequence, Parallel)
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process.Serializable (Serializable, SerializableDict)
import qualified Control.Distributed.Process as CH (Process, Closure, Static)

-------------------------------------------------------------------------------

data Process a b where
  Simple   :: (Serializable b) => CH.Static (SerializableDict b) -> (a -> CH.Closure (CH.Process b)) -> Process a b
  Sequence :: (Serializable b, Serializable c) => Process a c -> Process c b -> Process a b
  Parallel :: (Serializable b) => Process a b -> Process a b -> (b -> b -> b) -> Process a b

-------------------------------------------------------------------------------