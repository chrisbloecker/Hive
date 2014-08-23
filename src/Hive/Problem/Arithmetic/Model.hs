{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Hive.Problem.Arithmetic.Model
  where

-------------------------------------------------------------------------------

import Hive.Imports.MkBinary
import Hive.Imports.DeriveJSON

-------------------------------------------------------------------------------

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
  deriving (Generic, Typeable, Eq, Show, Read)

instance Binary Expr where

-------------------------------------------------------------------------------

$(deriveJSON defaultOptions ''Expr)

-------------------------------------------------------------------------------

eval :: Expr -> Int
eval (Val i) = i
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

-------------------------------------------------------------------------------

parse :: Text -> Maybe (Expr)
parse = decode' . encodeUtf8