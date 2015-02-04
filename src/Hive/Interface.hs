module Hive.Interface
  ( Master
  , Process (..)
  , BasicProcess
  , Problem (Problem)
  , ProblemType (..)
  , Solution (..)
  , runProcess
  ) where

import Hive.Types ( Master
                  )

import Hive.Process ( Process (..)
                    , BasicProcess
                    , runProcess
                    )

import Hive.Problem.Types ( Problem (Problem)
                          , ProblemType (..)
                          , Solution (..)
                          )