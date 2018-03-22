module Development.CI.Ofborg.Stats.FromServer.Wreq where

import Control.Lens (view)
import Data.ByteString.Lazy (toStrict)
import Network.Wreq (get, responseStatus, statusCode, responseBody)

import Development.CI.Ofborg.Stats.FromServer

-- | 'FetchEffects' implemented on top of wreq in 'IO'.
wreqFetchEffects :: FetchEffects String IO
wreqFetchEffects = FetchEffects
  { gET = get
  , resStatus = view $ responseStatus . statusCode
  , resBody = toStrict . view responseBody
  }
