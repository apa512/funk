{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Funk.LastFM
  ( scrobble
  ) where

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import Data.Digest.Pure.MD5 (MD5Digest, md5, md5DigestBytes)
import Data.List (sort)
import Funk.Track (PlayedTrack(..), Track(..))
import GHC.Generics (Generic)
import Hackery
import Network.HTTP.Simple

data Session = Session
  { subscriber :: Integer
  , name :: String
  , key :: String
  } deriving (Generic, Show)

data SessionResponse = SessionResponse
  { session :: Session
  } deriving (Generic, Show)

instance FromJSON Session

instance FromJSON SessionResponse

apiKey = $(compiledEnv "API_KEY")

apiSecret = $(compiledEnv "API_SECRET")

scrobble sk (PlayedTrack ts track) = do
  response <- httpLBS . setRequestBodyURLEncoded params . setSecurePost $ apiRequest
  let body = getResponseBody response
  B.appendFile "/home/erik/funk.log" $ B.concat (map (uncurry B.append) params)
  print body
  return ()
  where
    params =
      requestParams $
      (trackParams track) ++
      [("timestamp", (U.fromString $ show ts)), ("sk", sk), ("method", "track.scrobble")]

requestParams args = all ++ [("api_sig", sig), ("format", "json")]
  where
    sig = apiSig $ all
    all = args ++ defaults
    defaults = [("api_key", apiKey)]

apiSig :: [(B.ByteString, B.ByteString)] -> B.ByteString
apiSig args = U.fromString . show . md5 . L.fromStrict $ sig
  where
    sig :: B.ByteString
    sig = B.append (B.concat . map (uncurry B.append) . sort $ args) apiSecret

trackParams :: Track -> [(B.ByteString, B.ByteString)]
trackParams track =
  [ ("artist", U.fromString $ artist track)
  , ("track", U.fromString $ title track)
  , ("duration", U.fromString $ show $ duration track)
  ] ++
  optional
  where
    optional =
      [ (k, U.fromString v)
      | (k, Just v) <- [("album", album track), ("mbid", mbid track), ("trackNumber", fmap show $ trackNumber track)]
      ]

apiRequest = setURL defaultRequest

testRequest s = setRequestPath s . setRequestHost "requestb.in" $ defaultRequest

setSecurePost = setPost . setRequestPort 443 . setRequestSecure True

setPost = setRequestMethod "POST"

setURL = setRequestPath "/2.0" . setRequestHost "ws.audioscrobbler.com"
