{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric, DeriveDataTypeable #-}

module Hive.Node
  ( runNode
  , terminateNode
  ) where

-------------------------------------------------------------------------------

import Control.Distributed.Process

import Hive.Imports.MkBinary

import Hive.Types
import Hive.Master

-------------------------------------------------------------------------------
-- we will only react to these messages, all others can be thrown away
-------------------------------------------------------------------------------

data Terminate = Terminate deriving (Generic, Typeable)
instance Binary Terminate where

-------------------------------------------------------------------------------
-- helper functions to keep messages to nodes clean
-------------------------------------------------------------------------------

terminateNode :: ProcessId -> Process ()
terminateNode node = send node Terminate

-------------------------------------------------------------------------------
-- behaviour of a node
-------------------------------------------------------------------------------

runNode :: String -> String -> Int -> Process ()
runNode masterHost masterPort _nodeCount = do
  thisNode <- getSelfNode
  mMaster  <- findMaster masterHost masterPort (seconds 1)
  case mMaster of
    Nothing     -> liftIO . putStrLn $ "No master found... Terminating..."
    Just master -> do
      nodeUp master thisNode
      linkMaster master
      Terminate <- expect
      return ()