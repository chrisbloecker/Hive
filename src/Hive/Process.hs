{-# LANGUAGE GADTs, TemplateHaskell #-}

module Hive.Process
  ( Process (Simple, Sequence, Parallel)
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process.Serializable (Serializable)

import Control.Distributed.Process (Closure)

-------------------------------------------------------------------------------

data Process a b where
  Simple   :: (Serializable a, Serializable b) => (a -> Closure b) -> Process a b
  Sequence :: (Serializable a, Serializable b, Serializable c) => Process a c -> Process c b -> Process a b
  Parallel :: (Serializable a, Serializable b) => Process a b -> Process a b -> (b -> b -> b) -> Process a b

-------------------------------------------------------------------------------