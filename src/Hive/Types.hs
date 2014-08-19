{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Types
  ( Timeout (unTimeout)
  , milliseconds, seconds, minutes, hours
  , Host
  , Port
  , Problem (..)
  , ProblemType (..)  -- reexporting
  , Instance (..)
  , Solution (..)
  ) where

-------------------------------------------------------------------------------

import Hive.Problem.Types

-------------------------------------------------------------------------------

newtype Timeout = Timeout { unTimeout :: Int }  deriving (Eq, Show)

type Host = String
type Port = String

--data Task = Task (Closure (Process ())) deriving (Generic, Typeable)
--data T a = T (Closure (Process a)) deriving (Generic, Typeable)
--data Task a where
--  Task :: (Serializable a) => (SendPort a) -> (Closure a) -> Task a

--instance Generic  (Task a) where
--instance Typeable (Task a) where

--instance (Serializable a) => Binary (Task a) where
--  put (Task closure) = put (0 :: Word8) >> put closure
--  get = getWord8 >> liftM Task get

-------------------------------------------------------------------------------

milliseconds :: Int -> Timeout
milliseconds ms = Timeout (1000 * ms)

seconds :: Int -> Timeout
seconds s = milliseconds (1000 * s)

minutes :: Int -> Timeout
minutes m = seconds (60 * m)

hours :: Int -> Timeout
hours h = minutes (60 * h)