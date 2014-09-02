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
  , Solution (..)
  ) where

-------------------------------------------------------------------------------

import Data.Data

import Hive.Imports.MkBinary
import Hive.Problem.Types

import Control.Distributed.Process (ProcessId)

-------------------------------------------------------------------------------

type Host    = String
type Port    = String
type History = [Entry]

newtype Timeout = Timeout { unTimeout :: Int }  deriving (Eq, Show)
newtype Ticket  = Ticket  { unTicket  :: Int }  deriving (Eq, Ord, Data, Generic, Typeable)
newtype Master  = Master ProcessId deriving (Eq, Generic, Typeable)

data Entry = Entry { ticket   :: Ticket
                   , problem  :: Problem
                   , solution :: Maybe Solution
                   }
  deriving (Eq, Ord, Data, Generic, Typeable, Show)

-------------------------------------------------------------------------------

instance Show Master where
  show (Master pid) = show pid

instance Show Ticket where
  show (Ticket t) = show t

instance Binary Master where
instance Binary Entry where
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