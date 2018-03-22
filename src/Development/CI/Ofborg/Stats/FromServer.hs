{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
module Development.CI.Ofborg.Stats.FromServer where

import Control.Lens.Operators ((<&>))
import Data.Aeson (Value)
import Data.Aeson.Internal (iparse, JSONPath, IResult)
import Data.Aeson.Parser.Internal (eitherDecodeStrictWith, jsonEOF')
import Data.ByteString (ByteString)

import Development.CI.Ofborg.Stats
import Development.CI.Ofborg.Stats.FromJSON

-- | Effects needed to fetch from the server.
--
-- 'url': The type of URLs.
-- 'm': The monad we're operating in.
data FetchEffects url m = forall res . FetchEffects
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
                => FetchEffects url m
                -> url
                -> m (Either FetchError AllQueueStats)
fetchFromServer (FetchEffects {..}) url = gET url <&> \r ->
    case resStatus r of
      200 -> parse $ resBody r
      n -> Left $ HTTPFailure n
  where
    iparser :: Value -> IResult AllQueueStats
    iparser = iparse parseAllQueueStatsFromJSON

    parse :: ByteString -> Either FetchError AllQueueStats
    parse bs = case eitherDecodeStrictWith jsonEOF' iparser bs of
      Left (path, msg) -> Left $ ParseError path msg
      Right st -> Right st
