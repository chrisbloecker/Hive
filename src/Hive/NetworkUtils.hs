module Hive.NetworkUtils
  ( whereisRemote
  ) where

import Control.Monad (join)
import Network.Transport.TCP (encodeEndPointAddress)
import Control.Distributed.Process (Process, ProcessId, WhereIsReply(WhereIsReply), whereisRemoteAsync, receiveTimeout, match)
import Control.Distributed.Process.Internal.Types (NodeId(NodeId))
import Hive.Types (Timeout(unTimeout))

whereisRemote :: String -> String -> String -> Timeout -> Process (Maybe ProcessId)
whereisRemote host port name timeout = do
  whereisRemoteAsync (NodeId $ encodeEndPointAddress host port 0) name
  m <- receiveTimeout (unTimeout timeout) [ match $ \(WhereIsReply _ mPid) ->
                                              return mPid
                                          ]
  return (join m)