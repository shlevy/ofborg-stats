{-# LANGUAGE OverloadedStrings #-}
module Development.CI.Ofborg.Stats where

import Data.Map (Map)
import Data.Text (Text)

-- | Statistics about messages in a queue.
data QueueMessageStats = QueueMessageStats
  { -- | The number of messages in the queue.
    waiting :: !Integer
  , -- | The number of in-progress messages.
    inProgress :: !Integer
  }

-- | Statistics about a queue.
data QueueStats = QueueStats
  { -- | The name of the queue.
    queue_name :: !Text
  , -- | The number of queue consumers.
    queue_consumers :: !Integer
  , -- | Statistics about messages in the queue.
    queue_messages :: !QueueMessageStats
  }

-- | Statistics about all queues in a running ofborg instance
data AllQueueStats = AllQueueStats
  { -- | Statistics about the build queues, keyed by name
    allQueueStats_buildQueues :: !(Map Text QueueStats)
  , -- | Statistics about the evaluator queue
    allQueueStats_evaluator :: !QueueStats
  }
