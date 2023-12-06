#!/usr/bin/env stack
{- stack
 runghc
 --package attoparsec
 --package filepath
 --package pseudomacros
 --package split
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Attoparsec.Text hiding (take, takeWhile, D)

import Control.Monad (void, forM_, when)
import Control.Applicative ((<|>), empty)
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import Data.List
import qualified Data.Map.Lazy as DM
import qualified Data.Set as DS
import Data.Ord (comparing)
import Data.Char (chr, ord, toLower, isDigit)
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Traversable (for)

import Data.Time

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

-- fileParser = do
--     string "Time:"
--     many1 space
--     times <- sepBy1 decimal (many1 space)
--     char '\n'
--     string "Distance:"
--     many1 space
--     distances <- sepBy1 decimal (many1 space)
--     return (times, distances)

fileParser :: Parser (Integer, Integer)
fileParser = do
    string "Time:"
    many1 space
    time <- sepBy1 (many1 digit) (many1 space)
    traceShow time (return ())
    char '\n'
    string "Distance:"
    many1 space
    distance <- sepBy1 (many1 digit) (many1 space)
    return (read (concat time), read (concat distance))


runRace (time, bestDistance) = let winningTimes = filter (\holdTime -> (time - holdTime) * holdTime > bestDistance) [0..time] in length winningTimes

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    start <- getCurrentTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values@(time, distance) -> do
            print values

            -- let races = zip times distances
            --     results = map runRace races

            -- print results
            -- print (product results)

            let results = runRace (time, distance)

            print results

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop start))