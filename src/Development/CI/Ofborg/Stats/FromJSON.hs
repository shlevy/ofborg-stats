{-# LANGUAGE OverloadedStrings #-}
module Development.CI.Ofborg.Stats.FromJSON
  ( parseAllQueueStatsFromJSON
  ) where

import Data.Aeson.Types (Parser, Value, withObject,  (.:) )
import Data.HashMap.Strict (HashMap, traverseWithKey)
import Data.Text (Text, unpack)

import Development.CI.Ofborg.Stats

-- | Parse the ofborg JSON representation of all of the queue stats
-- for a given instance.
parseAllQueueStatsFromJSON :: Value -> Parser AllQueueStats
parseAllQueueStatsFromJSON = withObject "AllQueueStats" $ \v ->
  AllQueueStats <$>
  (parseBuildQueueStatsFromJSON =<< v .: "build-queues") <*>
  (parseQueueStatsFromJSON "evaluator" =<< v .: "evaluator")

-- | Parse the ofborg JSON representation of the map of build queue
-- stats for a given instance.
parseBuildQueueStatsFromJSON :: Value
                             -> Parser (HashMap Text QueueStats)
parseBuildQueueStatsFromJSON = withObject "build-queues" $
  traverseWithKey parseQueueStatsFromJSON

-- | Parse the ofborg JSON representation of a single queue's stats.
parseQueueStatsFromJSON :: Text -> Value -> Parser QueueStats
parseQueueStatsFromJSON key = withObject (unpack key) $ \v ->
  QueueStats <$>
  pure key <*>
  v .: "consumers" <*>
  (parseQueueMessageStatsFromJSON =<< v .: "messages")

-- | Parse the ofborg JSON representation of a queue's message stats.
parseQueueMessageStatsFromJSON :: Value -> Parser QueueMessageStats
parseQueueMessageStatsFromJSON = withObject "messages" $ \v ->
  QueueMessageStats <$>
  v .: "waiting" <*>
  v .: "in_progress"
