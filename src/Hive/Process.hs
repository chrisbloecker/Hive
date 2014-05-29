{-# LANGUAGE GADTs #-}

module Hive.Process
  ( Process (Simple, Sequence, Parallel, Loop)
  , runProcess
  , runProcessOn
  ) where

-------------------------------------------------------------------------------

import Prelude hiding ((.), id)
import Hive.Types (Scheduler)

import Data.Monoid      (Monoid, mappend)
import Control.Category (Category, (.), id)

-------------------------------------------------------------------------------

data Process a b where
  Simple   :: (a -> b) -> Process a b
  Sequence :: Process a c -> Process c b -> Process a b
  Parallel :: (Monoid b) => Process a b -> Process a b -> Process a b
  Loop     :: Process a a -> Int -> Process a a

instance Category Process where
  id = Simple id
  p1 . p0 = Sequence p0 p1

-------------------------------------------------------------------------------

runProcess :: Process a b -> a -> b
runProcess (Simple f)       x = f x
runProcess (Sequence p0 p1) x = runProcess p1 (runProcess p0 x)
runProcess (Parallel p0 p1) x = runProcess p0 x `mappend` runProcess p1 x
runProcess (Loop p n)       x | n <= 0    = x
                              | otherwise = runProcess (Loop p (n-1)) (runProcess p x)

runProcessOn :: Scheduler -> Process a b -> a -> b
runProcessOn _scheduler _p _x = undefined