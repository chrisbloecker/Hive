module Hive.Imports.DeriveJSON
  ( FromJSON, decode'
  , deriveJSON, defaultOptions
  , Text
  , encodeUtf8
  ) where

-------------------------------------------------------------------------------

import Data.Aeson    (FromJSON, decode')
import Data.Aeson.TH (deriveJSON, defaultOptions)

import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)