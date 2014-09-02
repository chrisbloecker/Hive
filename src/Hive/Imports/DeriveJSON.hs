module Hive.Imports.DeriveJSON
  ( FromJSON, decode'
  , deriveJSON, hiveJSONOptions
  , Text
  , encodeUtf8
  ) where

-------------------------------------------------------------------------------

import Data.Aeson    (FromJSON, decode')
import Data.Aeson.TH (Options (..), SumEncoding (..), deriveJSON)

import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

-------------------------------------------------------------------------------

hiveJSONOptions :: Options
hiveJSONOptions = Options { fieldLabelModifier      = id
                          , constructorTagModifier  = id
                          , allNullaryToStringTag   = True
                          , omitNothingFields       = False
                          , sumEncoding             = ObjectWithSingleField
                          }