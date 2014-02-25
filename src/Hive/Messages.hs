{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Messages
  where

import Data.Binary      (Binary, put, get)
import Data.Typeable    (Typeable)
import Data.Text        (Text)
import Data.Text.Binary ()
import Data.DeriveTH    (derive, makeBinary)
import GHC.Generics     (Generic)

import Hive.Types

-- messages for debugging
data IntMsg         = IntMsg { unIntMsg :: Int    }            deriving (Generic, Typeable, Show)
data ChrMsg         = ChrMsg { unChrMsg :: Char   }            deriving (Generic, Typeable, Show)
data StrMsg         = StrMsg { unStrMgs :: String }            deriving (Generic, Typeable, Show)
data TxtMsg         = TxtMsg { unTxtMsg :: Text   }            deriving (Generic, Typeable, Show)

-- messages from drones
data DRegisterAtQ   = DRegisterAtQ Drone                       deriving (Generic, Typeable, Show)
data DWorkRequestS  = DWorkRequestS Drone                      deriving (Generic, Typeable, Show)
data DWorkFinishedS = DWorkFinishedS Solution Client           deriving (Generic, Typeable, Show)

-- messages from clients
data CSolveProblemQ = CSolveProblemQ ClientRequest             deriving (Generic, Typeable, Show)

-- messages from scheduler
data SSolutionC     = SSolutionC Solution                      deriving (Generic, Typeable, Show)
data SWorkReplyD    = SWorkReplyD (Problem, Client)            deriving (Generic, Typeable, Show)

-- messages from queen
data QRegisteredD   = QRegisteredD Scheduler Logger            deriving (Generic, Typeable, Show)
data QWorkD         = QWorkD Problem                           deriving (Generic, Typeable, Show)
data QEnqueProblemS = QEnqueProblemS ClientRequest             deriving (Generic, Typeable, Show)


$(derive makeBinary ''IntMsg)
$(derive makeBinary ''ChrMsg)
$(derive makeBinary ''StrMsg)
$(derive makeBinary ''TxtMsg)

$(derive makeBinary ''DRegisterAtQ)
$(derive makeBinary ''DWorkRequestS)
$(derive makeBinary ''DWorkFinishedS)
$(derive makeBinary ''CSolveProblemQ)
$(derive makeBinary ''SSolutionC)
$(derive makeBinary ''SWorkReplyD)
$(derive makeBinary ''QRegisteredD)
$(derive makeBinary ''QWorkD)
$(derive makeBinary ''QEnqueProblemS)