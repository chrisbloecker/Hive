{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Messages
  (
-- debug messages
    IntMsg (..)
  , ChrMsg (..)
  , StrMsg (..)
  , TxtMsg (..)

-- messages from drones
  , DRegisterAtQ (..)
  , DWorkRequestS (..)
  , DAvailableS (..)

-- messages from clients
  , CSolveProblemQ (..)
  , CGetStatisticsQ (..)

-- messages from scheduler
  , SSolutionC (..)

-- messages from process creators
--  , PAddProcessS (..)

-- messages from queen
  , QRegisteredD (..)
  , QNewDroneS (..)
  , QDroneDisappearedS (..)
  , QWorkD (..)
  , QEnqueRequestS (..)
  ) where

-------------------------------------------------------------------------------

import Hive.Imports.MkBinary

import Data.Text        (Text)
import Data.Text.Binary ()

import Hive.Types

-------------------------------------------------------------------------------

-- debug messages
data IntMsg             = IntMsg Int                             deriving (Generic, Typeable)
data ChrMsg             = ChrMsg Char                            deriving (Generic, Typeable)
data StrMsg             = StrMsg String                          deriving (Generic, Typeable)
data TxtMsg             = TxtMsg Text                            deriving (Generic, Typeable)

-- messages from drones
data DRegisterAtQ       = DRegisterAtQ Drone                     deriving (Generic, Typeable)
data DWorkRequestS      = DWorkRequestS Drone                    deriving (Generic, Typeable)
data DAvailableS        = DAvailableS Drone                      deriving (Generic, Typeable)

-- messages from clients
data CSolveProblemQ     = CSolveProblemQ ClientRequest           deriving (Generic, Typeable)
data CGetStatisticsQ    = CGetStatisticsQ Client                 deriving (Generic, Typeable)

-- messages from scheduler
data SSolutionC         = SSolutionC Solution                    deriving (Generic, Typeable)

-- messages from queen
data QRegisteredD       = QRegisteredD Scheduler                 deriving (Generic, Typeable)
data QNewDroneS         = QNewDroneS Drone                       deriving (Generic, Typeable)
data QDroneDisappearedS = QDroneDisappearedS Drone               deriving (Generic, Typeable)
data QWorkD             = QWorkD Problem                         deriving (Generic, Typeable)
data QEnqueRequestS     = QEnqueRequestS ClientRequest           deriving (Generic, Typeable)

-------------------------------------------------------------------------------

$(derive makeBinary ''IntMsg)
$(derive makeBinary ''ChrMsg)
$(derive makeBinary ''StrMsg)
$(derive makeBinary ''TxtMsg)

$(derive makeBinary ''DRegisterAtQ)
$(derive makeBinary ''DWorkRequestS)
$(derive makeBinary ''DAvailableS)

$(derive makeBinary ''CSolveProblemQ)
$(derive makeBinary ''CGetStatisticsQ)

$(derive makeBinary ''SSolutionC)

$(derive makeBinary ''QRegisteredD)
$(derive makeBinary ''QNewDroneS)
$(derive makeBinary ''QDroneDisappearedS)
$(derive makeBinary ''QWorkD)
$(derive makeBinary ''QEnqueRequestS)