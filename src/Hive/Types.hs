{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Types
  ( Timeout (..)
  , milliseconds, seconds, minutes, hours
  , Host
  , Port
  , Master (..)
  , Time (..)
  , mkTime
  , diffTime
  , Ticket (..)
  , Entry (..)
  , mkEntry
  , History
  , mkTicket
  , Problem (..)
  , ProblemType (..)  -- reexporting
  , Solution (..)
  ) where

-------------------------------------------------------------------------------

import Data.Data
import Data.Time.Clock
import Data.Time.Calendar

import Hive.Imports.MkBinary
import Hive.Problem.Types

import Control.Distributed.Process (ProcessId)

-------------------------------------------------------------------------------

type Host    = String
type Port    = String
type History = [Entry]

newtype Timeout = Timeout { unTimeout :: Int } deriving (Eq, Show)
newtype Ticket  = Ticket  { unTicket  :: Int } deriving (Eq, Ord, Data, Generic, Typeable)
newtype Master  = Master ProcessId deriving (Eq, Generic, Typeable)

data Time = Time { getday :: Integer, getseconds :: Integer } deriving (Eq, Ord, Data, Generic, Typeable)

data Entry = Entry { ticket    :: Ticket
                   , problem   :: Problem
                   , solution  :: Maybe Solution
                   , startTime :: Time
                   , endTime   :: Maybe Time
                   }
  deriving (Eq, Ord, Data, Generic, Typeable, Show)

-------------------------------------------------------------------------------

instance Show Master where
  show (Master pid) = show pid

instance Show Ticket where
  show (Ticket t) = show t

instance Show Time where
  show (Time 0 s) = show s ++ "s"
  show (Time d s) = show $ Time 0 (60*60*24*d+s)

instance Binary Master where
instance Binary Entry where
instance Binary Ticket where
instance Binary Time where

-------------------------------------------------------------------------------

milliseconds :: Int -> Timeout
milliseconds ms = Timeout (1000 * ms)

seconds :: Int -> Timeout
seconds s = milliseconds (1000 * s)

minutes :: Int -> Timeout
minutes m = seconds (60 * m)

hours :: Int -> Timeout
hours h = minutes (60 * h)

mkTicket :: Int -> Ticket
mkTicket = Ticket

mkTime :: UTCTime -> Time
mkTime utcTime = Time (toModifiedJulianDay . utctDay $ utcTime) (floor . utctDayTime $ utcTime)

mkEntry :: Ticket -> Problem -> Maybe Solution -> Time -> Maybe Time -> Entry
mkEntry = Entry

diffTime :: Time -> Time -> Time
diffTime (Time d1 s1) (Time d2 s2) = Time (d2 - d1) (s2 - s1)