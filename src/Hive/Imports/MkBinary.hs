module Hive.Imports.MkBinary
  ( Binary, put, get, putWord8, getWord8
  , Typeable
  , derive, makeBinary
  , Generic
  ) where

-------------------------------------------------------------------------------

import Data.Binary      (Binary, put, get, putWord8, getWord8)
import Data.Typeable    (Typeable)
import Data.DeriveTH    (derive, makeBinary)
import GHC.Generics     (Generic)