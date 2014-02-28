{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hive.Scheduler.Strategy
  where

import Control.Monad

import Data.Sequence (Seq, (|>), (><), index, empty)

-------------------------------------------------------------------------------

class (Monad s, MonadPlus s) => Strategy s where
  enqueue :: a -> s a -> s a
  dequeue :: s a -> a

-------------------------------------------------------------------------------
-- FIFO Scheduler

newtype FIFO a = FIFO (Seq a)
  deriving (Monad)

instance MonadPlus FIFO where
  mzero = FIFO empty
  (FIFO f1) `mplus` (FIFO f2)= FIFO $ f1 >< f2

instance Strategy FIFO where
  enqueue e (FIFO s)= FIFO $ s |> e
  dequeue (FIFO s) = s `index` 1