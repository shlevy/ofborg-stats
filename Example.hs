{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.HashMap.Strict as M
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Development.CI.Ofborg.Stats
import Development.CI.Ofborg.Stats.FromServer
import Development.CI.Ofborg.Stats.FromServer.Wreq

url :: String
url = "https://events.nix.ci/stats.php"

die :: String -> IO ()
die msg = hPutStrLn stderr msg >> exitFailure

main :: IO ()
main = fetchFromServer wreqFetchEffects url >>= \case
  Left (HTTPFailure code) -> die
    ("Bad HTTP status " ++ show code)
  Left (ParseError path msg) -> die
    ("JSON parsing failure at " ++ (show path) ++ ": " ++ msg)
  Right aqs ->
    let bqs = allQueueStats_buildQueues aqs
    in case M.lookup "build-inputs-x86_64-linux" bqs of
      Nothing -> die "build-inputs-x86_64-linux queue missing"
      Just (QueueStats {..}) -> putStrLn . show $
        inProgress queue_messages
