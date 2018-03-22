{-# LANGUAGE RecordWildCards #-}
module Development.CI.Ofborg.Stats.FromServer where

import Data.Aeson (Value)
import Data.Aeson.Internal (iparse, JSONPath, IResult)
import Data.Aeson.Parser.Internal (eitherDecodeStrictWith, jsonEOF')
import Data.ByteString (ByteString)

import Development.CI.Ofborg.Stats
import Development.CI.Ofborg.Stats.FromJSON

-- | Effects needed to fetch from the server.
--
-- 'url': The type of URLs.
-- 'res': The type of HTTP responses.
-- 'm': The monad we're operating in.
data FetchEffects url res m = FetchEffects
  { -- | GET the 'url'
    gET :: !(url -> m res)
  , -- | The HTTP status code of a response.
    resStatus :: !(res -> Int)
  , -- | The body of a response.
    resBody :: !(res -> ByteString)
  }

-- | Possible errors during fetching
data FetchError
  = HTTPFailure !Int -- ^ A non-200 HTTP status code.
  | ParseError !JSONPath !String -- ^ A JSON parsing error at the
                                 -- given path with the given message.

-- | Fetch all of the queue stats from a given URL.
fetchFromServer :: (Functor m)
                => FetchEffects url res m
                -> url
                -> m (Either FetchError AllQueueStats)
fetchFromServer (FetchEffects {..}) url = gET url <&> \r ->
    case resStatus r of
      200 -> parse $ resBody r
      n -> Left $ HTTPFailure n
  where
    -- Avoid lens just for this...
    (<&>) :: Functor f => f a -> (a -> b) -> f b
    (<&>) = flip fmap

    iparser :: Value -> IResult AllQueueStats
    iparser = iparse parseAllQueueStatsFromJSON

    parse :: ByteString -> Either FetchError AllQueueStats
    parse bs = case eitherDecodeStrictWith jsonEOF' iparser bs of
      Left (path, msg) -> Left $ ParseError path msg
      Right st -> Right st
