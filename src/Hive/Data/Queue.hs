module Hive.Data.Queue
  ( Queue
  , mkEmpty
  , isEmpty
  , insert
  , peak
  , size
  , remove
  )
  where

-------------------------------------------------------------------------------

data Queue a = Queue [a] [a]

-------------------------------------------------------------------------------

mkEmpty :: Queue a
mkEmpty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty             _ = False

insert :: a -> Queue a -> Queue a
insert e (Queue outL inL) = Queue outL (e:inL)

peak :: Queue a -> (a, Queue a)
peak (Queue   [] inL) = peak (Queue (reverse inL) [])
peak (Queue outL inL) = (head outL, Queue (tail outL) inL)

size :: Queue a -> Int
size (Queue outL inL) = length outL + length inL

remove :: (Eq a) => a -> Queue a -> Queue a
remove e (Queue outL inL) = Queue (filter' outL) (filter' inL)
  where
    filter' = filter (/= e)