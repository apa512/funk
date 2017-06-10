{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import qualified Funk.CLI as CLI

handleArgs :: [String] -> IO ()
handleArgs ["init"] = CLI.handleInit
handleArgs args@("status":"playing":_) = CLI.handlePlaying args
handleArgs _ = putStrLn "?"

main :: IO ()
main = getArgs >>= handleArgs
