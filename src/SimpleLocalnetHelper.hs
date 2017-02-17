
{-# LANGUAGE OverloadedStrings #-}

module SimpleLocalnetHelper
       (getSlaveBackend,
        getSlaveConfBackend,
        getMasterBackend,
        getMasterConfBackend,
        startMasterProcess) where

import System.IO
import qualified Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Node as Node
import Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Data.ByteString.Char8 as BS
import Network.Transport (EndPointAddress(..))

-- | Make a NodeId from "host:port" string.
makeNodeId :: String -> DP.NodeId
makeNodeId addr = DP.NodeId . EndPointAddress . BS.concat $ [BS.pack addr, ":0"]

splitAddr :: String -> (String, String)
splitAddr addr = (host, port)
  where
    (host, rest) = break (== ':') addr
    port = tail rest

deleteAt :: Int -> [String] -> [String]
deleteAt i xs = xs1 ++ xs2
  where (xs1, rest) = splitAt i xs
        xs2 = tail rest

getSlaveBackend :: Int -> [String] -> DP.RemoteTable -> IO Backend
getSlaveBackend i addrs rtable =
  do let addr = addrs !! i
         (host, port) = splitAddr addr
     backend <- initializeBackend host port rtable
     return backend

getMasterBackend :: Int -> [String] -> DP.RemoteTable -> IO (Backend, [DP.NodeId])
getMasterBackend i addrs rtable =
  do let addr = addrs !! i
         (host, port) = splitAddr addr
         slaves = map makeNodeId $ deleteAt i addrs 
     backend <- initializeBackend host port rtable
     return (backend, slaves)

getClusterConf :: IO [String]
getClusterConf = fmap addresses $ readFile "cluster.conf"

addresses :: String -> [String]
addresses =
  filter (\x -> head x /= '#') .
  filter (\x -> not $ null $ filter (\y -> y /= ' ') x) .
  lines

getSlaveConfBackend :: Int -> DP.RemoteTable -> IO Backend
getSlaveConfBackend i rtable =
  do addrs <- getClusterConf
     getSlaveBackend i addrs rtable

getMasterConfBackend :: Int -> DP.RemoteTable -> IO (Backend, [DP.NodeId])
getMasterConfBackend i rtable =
  do addrs <- getClusterConf
     getMasterBackend i addrs rtable

startMasterProcess :: Backend -> [DP.NodeId] -> (Backend -> [DP.NodeId] -> DP.Process ()) -> IO ()
startMasterProcess backend slaves process =
  do node <- newLocalNode backend
     Node.runProcess node (process backend slaves)
