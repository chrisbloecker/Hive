{-# LANGUAGE TemplateHaskell #-}

module Main
  where

import System.Environment (getArgs)
--import Control.Distributed.Process

import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node hiding (newLocalNode)

remotable []

startQueen = undefined

startDrone = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["queen", host, port] -> do
      context <- initializeBackend host port $ __remoteTable initRemoteTable
      node    <- newLocalNode context

      runProcess node (startQueen context)

    ["drone", host, port] -> do
      context <- initializeBackend host port $ __remoteTable initRemoteTable
      node    <- newLocalNode context

      runProcess node (startDrone context)

    other -> do
      putStrLn $ "Your arguments are invalid: " ++ show other