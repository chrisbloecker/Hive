{-# LANGUAGE TemplateHaskell #-}

-- | The hello world for interpreters combined with how to use the Hive processes algebra.
--
-- Using the Hive process algebra will look similar to this:
--
-- * define a data type that describes the computations to be carried out (in this case 'Expr')
--
-- * define basic (pure) functions that will be used to carry out computations on the defined data type
-- or its components and wrap them into Cloud Haskell processes
--
-- * wrap the cloud haskell processes into Hive processes (typically by using the 'Hive.Process.Const'
-- and 'Hive.Process.Simple' process creators from "Hive.Process")
--
-- * implement an interpreter function that takes a value of the data type that should be processed
-- ('Expr') and creates a Hive process representing the desired computation (using the process combinators
-- 'Hive.Process.Local', 'Hive.Process.Choice', 'Hive.Process.Sequence', 'Hive.Process.Parallel'
-- , 'Hive.Process.Multilel' and 'Hive.Process.Loop' from "Hive.Process")
module Hive.Problem.Arithmetic
  ( __remoteTable
  , Expr (..)
  , parse
  , eval
  , val
  , add
  , subtract
  , multiply
  , divide
  , intDict
  , valProcess
  , addProcess
  , subtractProcess
  , multiplyProcess
  , divideProcess
  , interpret
  ) where

-------------------------------------------------------------------------------

import Prelude hiding (subtract)

import Control.Distributed.Process.Closure      (mkClosure, mkStatic, remotable)
import Control.Distributed.Process.Serializable (SerializableDict(SerializableDict))

import Hive.Interface
import Hive.Imports.DeriveJSON

import qualified Control.Distributed.Process as CH (Process)

-------------------------------------------------------------------------------
-- the data model
-------------------------------------------------------------------------------

-- | Arithmetic expressions
data Expr = Val Int -- ^ values
          | Add Expr Expr -- ^ addition
          | Sub Expr Expr -- ^ subtractions
          | Mul Expr Expr -- ^ multiplications
          | Div Expr Expr -- ^ division
  deriving (Eq, Show, Read)

$(deriveJSON hiveJSONOptions ''Expr)

-------------------------------------------------------------------------------

-- | Reads a text and maybe procudes an expression
parse :: Text -> Maybe Expr
parse = decode' . encodeUtf8

-- | Evaluates a given expression
eval :: Expr -> Int
eval (Val i) = i
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | Takes an Int and creates a Cloud Haskell process which returns this Int
--
-- > val i = return i
val :: Int -> CH.Process Int
val i = return i

-- | Takes a pair of Ints and creates a Cloud Haskell process which adds them
--
-- > add (x, y) = return (x + y)
add :: (Int, Int) -> CH.Process Int
add (x, y) = return (x + y)

-- | Takes a pair of Ints and creates a Cloud Haskell process which subtracts the second from the first
--
-- > subtract (x, y) = return (x - y)
subtract :: (Int, Int) -> CH.Process Int
subtract (x, y) = return (x - y)

-- | Takes a pair of Ints and creates a Cloud Haskell process which multiplies them
multiply :: (Int, Int) -> CH.Process Int
multiply (x, y) = return (x * y)

-- | Takes a pair of Ints and creates a Cloud Haskell process which divides the first by the second
--
-- Yes, division by 0 is undefined.
--
-- > divide (_, 0) = undefined
-- > divide (x, y) = return (x `div` y)
divide :: (Int, Int) -> CH.Process Int
divide (_, 0) = undefined
divide (x, y) = return (x `div` y)

-- | A SerializableDict for Int.
-- This is neccessary for wrapping the previously defined Cloud Haskell processes into Hive processes.
--
-- > intDict = SerializableDict
intDict :: SerializableDict Int
intDict = SerializableDict

-------------------------------------------------------------------------------

-- We need to make all of these remotable and export the generated __remoteTable so we can spawn these on other nodes later.
remotable ['val, 'add, 'subtract, 'multiply, 'divide, 'intDict]

-------------------------------------------------------------------------------

-- | Takes an Int and creates a Hive process which returns this Int based on 'val'
--
-- > valProcess i = mkConst $(mkStatic 'intDict) ($(mkClosure 'val) i)
valProcess :: Int -> Process () Int
valProcess i = Const $(mkStatic 'intDict) ($(mkClosure 'val) i)

-- | Creates a Hive process that expects a pair of Ints and adds them, based on 'add'
--
-- > addProcess = mkSimple $(mkStatic 'intDict) $(mkClosure 'add)
addProcess :: Process (Int, Int) Int
addProcess = Simple $(mkStatic 'intDict) $(mkClosure 'add)

-- | Creates a Hive process that expects a pair of Ints and subtracts the second from the first, based on 'subtract'
--
-- > subtractProcess = mkSimple $(mkStatic 'intDict) $(mkClosure 'subtract)
subtractProcess :: Process (Int, Int) Int
subtractProcess = Simple $(mkStatic 'intDict) $(mkClosure 'subtract)

-- | Creates a Hive process that expects a pair of Ints and multiplies them, based on 'multiply'
--
-- > multiplyProcess = mkSimple $(mkStatic 'intDict) $(mkClosure 'multiply)
multiplyProcess :: Process (Int, Int) Int
multiplyProcess = Simple $(mkStatic 'intDict) $(mkClosure 'multiply)

-- | Creates a Hive process that expects a pair of Ints and divides the first by the second, based on 'divide'
--
-- > divideProcess = mkSimple $(mkStatic 'intDict) $(mkClosure 'divide)
divideProcess :: Process (Int, Int) Int
divideProcess = Simple $(mkStatic 'intDict) $(mkClosure 'divide)

-------------------------------------------------------------------------------
-- interpretation of an Expr in form of a Process
-------------------------------------------------------------------------------

-- | The interpreter for 'Expr'. It translates a given 'Expr' into a Hive process and exploits the fact
-- that for any operation the operands can be computed in parallel.
interpret :: Expr -> Process () Int
interpret (Val i)   = valProcess i
interpret (Add x y) = Parallel (interpret x) (interpret y) addProcess
interpret (Sub x y) = Parallel (interpret x) (interpret y) subtractProcess
interpret (Mul x y) = Parallel (interpret x) (interpret y) multiplyProcess
interpret (Div x y) = Parallel (interpret x) (interpret y) divideProcess