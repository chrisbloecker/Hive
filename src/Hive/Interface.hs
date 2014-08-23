module Hive.Interface
  ( Master
  , Process
  , Problem (Problem)
  , ProblemType (..)
  , Solution (..)
  , Instance (unInstance)
  , runProcess
  , mkConst
  , mkSimple
  , mkChoice
  , mkSequence
  , mkParallel
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
                    )

import Hive.Problem.Types ( Problem (Problem)
                          , ProblemType (..)
                          , Solution (..)
                          , Instance (unInstance)
                          )