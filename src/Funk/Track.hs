{-# LANGUAGE DeriveGeneric #-}

module Funk.Track
  ( PlayedTrack(..)
  , Track(..)
  , fromList
  ) where

import Data.Aeson
import qualified Data.HashMap as M
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data Track = Track
  { artist :: String
  , title :: String
  , album :: Maybe String
  , duration :: Integer
  , trackNumber :: Maybe Integer
  , mbid :: Maybe String
  } deriving (Eq, Generic, Show)

data PlayedTrack = PlayedTrack
  { timestamp :: Integer
  , track :: Track
  } deriving (Generic, Show)

instance Eq PlayedTrack where
  x == y = track x == track y

instance FromJSON Track

instance ToJSON Track

instance FromJSON PlayedTrack

instance ToJSON PlayedTrack

fromList :: [(String, String)] -> Maybe Track
fromList args =
  Track <$> get "artist" <*> get "title" <*> pure (get "album") <*> (get "duration" >>= readMaybe) <*>
  pure (get "tracknumber" >>= readMaybe) <*>
  pure (get "musicbrainz_trackid")
  where
    get k = M.lookup k m
    m = M.fromList args
