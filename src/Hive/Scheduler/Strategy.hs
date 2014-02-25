{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hive.Scheduler.Stretegy
  where

import Data.Sequence (Seq, (|>), index)

-------------------------------------------------------------------------------

class (Monad s) => Strategy s where
  enqueue :: a -> s a -> s a
  dequeue :: s a -> a

newtype FIFO a = FIFO { unFIFO :: Seq a }
  deriving (Eq, Show, Monad)

instance Strategy FIFO where
  enqueue e fifo = FIFO $ unFIFO fifo |> e
  dequeue        = flip index 1 . unFIFO