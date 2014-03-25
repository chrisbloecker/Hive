{-# LANGUAGE OverloadedStrings #-}

module Hive.Drone
  ( runDrone
  ) where

-------------------------------------------------------------------------------

import System.Process  (readProcess)

import Data.ByteString.Char8 (pack)

import Network.Transport (EndPointAddress(..))
import Control.Distributed.Process (Process, WhereIsReply(..), getSelfPid, link, send, expect,receiveWait, match, matchUnknown, unClosure, liftIO, say, whereisRemoteAsync)
import Control.Distributed.Process.Internal.Types (NodeId(..))

-------------------------------------------------------------------------------

import Hive.Types                  ( Queen
                                   , Scheduler
                                   , Logger
                                   , Task (..)
                                   )
import Hive.Messages               ( QRegisteredD (..)
                                   , DRegisterAtQ (..)
                                   , SWorkReplyD (..)
                                   , DWorkRequestS (..)
                                   , DAvailableS (..)
                                   , StrMsg (..)
                                   )

-------------------------------------------------------------------------------

data DroneState = DroneState Queen Scheduler Logger
  deriving (Show)

-------------------------------------------------------------------------------

runDrone :: String -> Process ()
runDrone queenAddr = do
  dronePid <- getSelfPid
  whereisRemoteAsync (NodeId . EndPointAddress $ pack queenAddr) "queen"
  (WhereIsReply _ queenPid) <- expect
  case queenPid of
    Just queen -> do
      cpuInfo <- liftIO $ readProcess "grep" ["model name", "/proc/cpuinfo"] ""
      send queen $ DRegisterAtQ dronePid (tail . dropWhile (/= ':') . takeWhile (/= '\n') $ cpuInfo)
      QRegisteredD scheduler logger <- expect
      link queen
      loop $ DroneState queen scheduler logger
    Nothing -> liftIO . putStrLn $ "No Queen found... Terminating..."
  where
    loop :: DroneState -> Process ()
    loop state@(DroneState queen scheduler _logger) = do
      send queen $ StrMsg "Entering drone loop..."
      dronePid <- getSelfPid
      send scheduler $ DWorkRequestS dronePid
      receiveWait [ match $ \(SWorkReplyD (Task closure)) -> do
                      send queen $ StrMsg "A drone got work..."
                      proc <- unClosure closure
                      send queen $ StrMsg "Closure unpacked..."
                      proc
                      send queen $ StrMsg "Closure executed..."
                      send scheduler $ DAvailableS dronePid
                      loop state
                  
                  , matchUnknown $ do
                      say "Unknown message received. Discarding..."
                      loop state
                  ]