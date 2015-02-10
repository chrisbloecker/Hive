module Hive.Interface
  ( Master
  , Process (..)
  , BasicProcess
  , Predicate
  , Problem (Problem)
  , ProblemType (..)
  , Solution (..)
  , runProcess
  ) where

import Hive.Types ( Master
                  )

import Hive.Process ( Process (..)
                    , BasicProcess
                    , Predicate
                    , runProcess
                    )

import Hive.Problem.Types ( Problem (Problem)
                          , ProblemType (..)
                          , Solution (..)
                          )