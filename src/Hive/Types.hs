{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Types
  ( Timeout (unTimeout)
  , milliseconds, seconds, minutes, hours
  , Host
  , Port
  , Master (..)
  , Ticket (unTicket)
  , Entry (ticket, problem, solution)
  , mkEntry
  , History
  , mkTicket
  , Problem (..)
  , ProblemType (..)  -- reexporting
  , Instance (..)
  , Solution (..)
  ) where

-------------------------------------------------------------------------------

import Data.Data

import Hive.Imports.MkBinary
import Hive.Problem.Types

import Control.Distributed.Process (ProcessId)

-------------------------------------------------------------------------------

newtype Timeout = Timeout { unTimeout :: Int }  deriving (Eq, Show)

type Host = String
type Port = String

data Entry = Entry { ticket   :: Ticket
                   , problem  :: Problem
                   , solution :: Maybe Solution
                   }
  deriving (Eq, Ord, Data, Generic, Typeable, Show)

instance Binary Entry where

type History = [Entry]

newtype Master = Master ProcessId deriving (Eq, Generic, Typeable)

instance Show Master where
  show (Master pid) = show pid
instance Binary Master where

newtype Ticket = Ticket { unTicket :: Int } deriving (Eq, Ord, Data, Generic, Typeable)

instance Show Ticket where
  show (Ticket t) = show t
instance Binary Ticket where

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

mkEntry :: Ticket -> Problem -> Maybe Solution -> Entry
mkEntry = Entry