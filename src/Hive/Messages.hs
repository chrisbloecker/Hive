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
  , SWorkReplyD (..)
  , SYourDronesW (..)

-- messages from process creators
-- no, we don't have any, check below

-- messages from queen
  , QRegisteredD (..)
  , QNewDroneS (..)
  , QDroneDisappearedS (..)
  , QWorkD (..)
  , QEnqueRequestS (..)
  , QStatisticsC (..)
  ) where

-------------------------------------------------------------------------------

import Data.Binary      (Binary, put, get)
import Data.Typeable    (Typeable)
import Data.Text        (Text)
import Data.Text.Binary ()
import Data.DeriveTH    (derive, makeBinary)
import GHC.Generics     (Generic)

import Hive.Types

-------------------------------------------------------------------------------

-- debug messages
data IntMsg             = IntMsg Int                             deriving (Generic, Typeable, Show)
data ChrMsg             = ChrMsg Char                            deriving (Generic, Typeable, Show)
data StrMsg             = StrMsg String                          deriving (Generic, Typeable, Show)
data TxtMsg             = TxtMsg Text                            deriving (Generic, Typeable, Show)

-- messages from drones
data DRegisterAtQ       = DRegisterAtQ Drone String              deriving (Generic, Typeable, Show)
data DWorkRequestS      = DWorkRequestS Drone                    deriving (Generic, Typeable, Show)
data DAvailableS        = DAvailableS Drone                      deriving (Generic, Typeable, Show)

-- messages from clients
data CSolveProblemQ     = CSolveProblemQ ClientRequest           deriving (Generic, Typeable, Show)
data CGetStatisticsQ    = CGetStatisticsQ Client                 deriving (Generic, Typeable, Show)

-- messages from scheduler
data SSolutionC         = SSolutionC Solution                    deriving (Generic, Typeable, Show)
data SWorkReplyD        = SWorkReplyD Task                       deriving (Generic, Typeable, Show)
data SYourDronesW       = SYourDronesW [Drone]                   deriving (Generic, Typeable, Show)

-- messages from process creator
-- none so far, process creators should create process chains

-- messages from queen
data QRegisteredD       = QRegisteredD Scheduler Logger          deriving (Generic, Typeable, Show)
data QNewDroneS         = QNewDroneS Drone                       deriving (Generic, Typeable, Show)
data QDroneDisappearedS = QDroneDisappearedS Drone               deriving (Generic, Typeable, Show)
data QWorkD             = QWorkD Problem                         deriving (Generic, Typeable, Show)
data QEnqueRequestS     = QEnqueRequestS ClientRequest           deriving (Generic, Typeable, Show)
data QStatisticsC       = QStatisticsC Statistics                deriving (Generic, Typeable, Show)

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
$(derive makeBinary ''SWorkReplyD)
$(derive makeBinary ''SYourDronesW)

$(derive makeBinary ''QRegisteredD)
$(derive makeBinary ''QNewDroneS)
$(derive makeBinary ''QDroneDisappearedS)
$(derive makeBinary ''QWorkD)
$(derive makeBinary ''QEnqueRequestS)
$(derive makeBinary ''QStatisticsC)