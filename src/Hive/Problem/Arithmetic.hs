{-# LANGUAGE TemplateHaskell #-}

module Hive.Problem.Arithmetic
  ( __remoteTable
  , Expr (..)
  , parse
  , interpret
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process.Closure      (mkClosure, mkStatic, remotable)
import Control.Distributed.Process.Serializable (SerializableDict(SerializableDict))

import Hive.Interface
import Hive.Imports.DeriveJSON

import qualified Control.Distributed.Process as CH (Process)

-------------------------------------------------------------------------------
-- the data model
-------------------------------------------------------------------------------

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
  deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''Expr)

parse :: String -> Expr
parse = read

-------------------------------------------------------------------------------

eval :: Expr -> Int
eval (Val i) = i
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

val :: Int -> CH.Process Int
val i = return i

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

remotable ['val, 'add, 'subtract, 'multiply, 'divide, 'intDict]

-------------------------------------------------------------------------------

valProcess :: Int -> Process Int Int
valProcess i = mkConst $(mkStatic 'intDict) ($(mkClosure 'val) i)

addProcess :: Process (Int, Int) Int
addProcess = mkSimple $(mkStatic 'intDict) $(mkClosure 'add)

subtractProcess :: Process (Int, Int) Int
subtractProcess = mkSimple $(mkStatic 'intDict) $(mkClosure 'subtract)

multiplyProcess :: Process (Int, Int) Int
multiplyProcess = mkSimple $(mkStatic 'intDict) $(mkClosure 'multiply)

divideProcess :: Process (Int, Int) Int
divideProcess = mkSimple $(mkStatic 'intDict) $(mkClosure 'divide)

-------------------------------------------------------------------------------
-- interpretation of an Expr in form of a Process
-------------------------------------------------------------------------------

interpret :: Expr -> Process Int Int
interpret (Val i)   = valProcess i
interpret (Add x y) = mkParallel (interpret x) (interpret y) addProcess
interpret (Sub x y) = mkParallel (interpret x) (interpret y) subtractProcess
interpret (Mul x y) = mkParallel (interpret x) (interpret y) multiplyProcess
interpret (Div x y) = mkParallel (interpret x) (interpret y) divideProcess