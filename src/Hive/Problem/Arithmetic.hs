{-# LANGUAGE TemplateHaskell #-}

module Hive.Problem.Arithmetic
  ( __remoteTable
  , addProcess
  , subtractProcess
  , multiplyProcess
  , divideProcess
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process.Closure      (mkClosure, mkStatic, remotable)
import Control.Distributed.Process.Serializable (SerializableDict(SerializableDict))

import qualified Control.Distributed.Process as CH (Process)

import Hive.Process (Process(Simple))

-------------------------------------------------------------------------------

add :: (Int, Int) -> CH.Process Int
add (x, y) = return (x + y)

subtract :: (Int, Int) -> CH.Process Int
subtract (x, y) = return (x - y)

multiply :: (Int, Int) -> CH.Process Int
multiply (x, y) = return (x * y)

divide :: (Int, Int) -> CH.Process Int
divide (_, 0) = undefined
divide (x, y) = return (x `div` y)

intDict :: SerializableDict Int
intDict = SerializableDict

-------------------------------------------------------------------------------

remotable ['add, 'subtract, 'multiply, 'divide, 'intDict]

-------------------------------------------------------------------------------

addProcess :: Process (Int, Int) Int
addProcess = Simple $(mkStatic 'intDict) $(mkClosure 'add)

subtractProcess :: Process (Int, Int) Int
subtractProcess = Simple $(mkStatic 'intDict) $(mkClosure 'subtract)

multiplyProcess :: Process (Int, Int) Int
multiplyProcess = Simple $(mkStatic 'intDict) $(mkClosure 'multiply)

divideProcess :: Process (Int, Int) Int
divideProcess = Simple $(mkStatic 'intDict) $(mkClosure 'divide)