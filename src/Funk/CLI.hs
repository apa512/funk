{-# LANGUAGE OverloadedStrings #-}

module Funk.CLI
  ( getPending
  , handlePlaying
  ) where

import Control.Monad (when, unless)
import Data.Aeson (encode, decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified System.Directory as D

import Funk.LastFM
import qualified Funk.Track as T

confDir :: IO FilePath
confDir = D.getXdgDirectory D.XdgConfig "funk"

confFile :: String -> IO String
confFile s = do
  dir <- confDir
  return $ dir ++ "/" ++ s

getTimestamp :: IO Integer
getTimestamp = round <$> getPOSIXTime

getSessionKey :: IO (Maybe B.ByteString)
getSessionKey = do
  file <- confFile "auth"
  fileExists <- D.doesFileExist file
  if fileExists
    then fmap (Just . UTF8.fromString) (readFile file)
    else return Nothing

getPending :: IO (Maybe T.PlayedTrack)
getPending = do
  file <- confFile "pending"
  fileExists <- D.doesFileExist file
  if fileExists
    then decode <$> (L.readFile file)
    else return Nothing

setPending :: T.PlayedTrack -> IO ()
setPending pt = do
  current <- getPending
  unless (Just pt == current) $ do
    file <- confFile "pending"
    L.writeFile file (encode pt)

scrobblePending sk pt = do
  pending <- getPending
  when (shouldScrobble pending pt) (scrobble sk (fromJust pending))
  where
    shouldScrobble (Just (T.PlayedTrack ts1 t1)) (T.PlayedTrack ts2 t2) =
      ts2 - ts1 >= (T.duration t1) - 10
    shouldScrobble _ _ = False

handlePlaying :: [String] -> IO ()
handlePlaying args = do
  sk <- getSessionKey
  let track = T.fromList $ pairs args
  case (sk, track) of
    (Just sk, Just track) -> do
      ts <- getTimestamp
      let pt = T.PlayedTrack ts track
      scrobblePending sk pt
      setPending pt
    (Just _, Nothing) -> putStrLn "Bad track!"
    _ -> putStrLn "No session key"
  where
    pairs (k:v:t) = (k, v) : pairs t
    pairs _ = []
