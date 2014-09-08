module Hive.Interface
  ( Master
  , Process
  , Problem (Problem)
  , ProblemType (..)
  , Solution (..)
  , runProcess
  , mkConst
  , mkSimple
  , mkChoice
  , mkSequence
  , mkParallel
  , mkMultilel
  , mkLoop
  , mkSimpleLoop
  ) where

import Hive.Types ( Master
                  )

import Hive.Process ( Process
                    , runProcess
                    , mkConst
                    , mkSimple
                    , mkChoice
                    , mkSequence
                    , mkParallel
                    , mkMultilel
                    , mkLoop
                    , mkSimpleLoop
                    )

import Hive.Problem.Types ( Problem (Problem)
                          , ProblemType (..)
                          , Solution (..)
                          )