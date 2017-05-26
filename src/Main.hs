{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import qualified Funk.CLI as CLI
import Funk.LastFM
import Funk.Track

handleArgs args@("status":"playing":_) = CLI.handlePlaying args
handleArgs _ = putStrLn "?"

main :: IO ()
main = getArgs >>= handleArgs
