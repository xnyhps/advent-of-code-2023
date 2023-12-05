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

data Interval = Interval
    { start :: Integer, len :: Integer}
    deriving (Show)

parseSeeds = do
    string "seeds: "
    seeds <- sepBy1 decimal space
    string "\n\n"
    return seeds

parseMap = do
    from <- many1 letter
    string "-to-"
    to <- many1 letter
    string " map:\n"
    m <- sepBy1 (do
        destRangeStart <- decimal
        space
        sourceRangeStart <- decimal
        space
        rangeLength <- decimal
        return (destRangeStart, sourceRangeStart, rangeLength)) (char '\n')
    return m

fileParser = do
    seeds <- parseSeeds

    maps <- sepBy1 parseMap (string "\n\n")

    return (seeds, maps)

-- convert [] number = number
-- convert ((destRangeStart, sourceRangeStart, rangeLength):maps) number | number >= sourceRangeStart, number < sourceRangeStart + rangeLength = destRangeStart + (number - sourceRangeStart)
-- convert (_:maps) number = convert maps number

convert :: [(Integer, Integer, Integer)] -> Interval -> [Interval]
convert [] interval = [interval]
-- Before the interval
convert ((_, sourceRangeStart, rangeLength):maps) interval@(Interval {..})
    | start + len < sourceRangeStart = convert maps interval
-- After the interval
convert ((_, sourceRangeStart, rangeLength):maps) interval@(Interval {..})
    | start > sourceRangeStart + rangeLength = convert maps interval
-- Entirely included in the map
convert ((destRangeStart, sourceRangeStart, rangeLength):maps) interval@(Interval {..})
    | start >= sourceRangeStart, start + len <= sourceRangeStart + rangeLength = [Interval { start = destRangeStart + (start - sourceRangeStart), len = len }]
-- Overlaps left
convert ((destRangeStart, sourceRangeStart, rangeLength):maps) interval@(Interval {..})
    | start >= sourceRangeStart, start <= sourceRangeStart + rangeLength, start + len > sourceRangeStart + rangeLength = (Interval (destRangeStart + (start - sourceRangeStart)) (sourceRangeStart + rangeLength - start)) : (convert maps (Interval (sourceRangeStart + rangeLength) (len - ((sourceRangeStart + rangeLength) - start))))
-- Overlaps right
convert ((destRangeStart, sourceRangeStart, rangeLength):maps) interval@(Interval {..})
    | start <= sourceRangeStart, start + len >= sourceRangeStart, start + len <= sourceRangeStart + rangeLength = (Interval destRangeStart (len - (sourceRangeStart - start))) : (convert maps (Interval start (sourceRangeStart - start)))
-- Map entirely included in the interval
convert ((destRangeStart, sourceRangeStart, rangeLength):maps) interval@(Interval {..})
    | start <= sourceRangeStart, start + len >= sourceRangeStart + rangeLength = [Interval destRangeStart rangeLength] ++ convert maps (Interval start (sourceRangeStart - start)) ++ convert maps (Interval (sourceRangeStart + rangeLength) (len - (sourceRangeStart + rangeLength)))
convert x@((destRangeStart, sourceRangeStart, rangeLength):maps) interval@(Interval {..}) = error ("Impossible: " ++ show (x,interval) ++ " " ++ show (start > sourceRangeStart, start < sourceRangeStart + rangeLength, start + len, sourceRangeStart + rangeLength))


main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    start <- getCurrentTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values@(seeds, maps) -> do
            print values

            -- let result = foldl (\values map_ -> map (convert map_) values) seeds (map (\(_,_,x) -> x) maps)

            -- print result

            -- print (minimum result)

            let seeds_ = f seeds
                f (x:y:rest) = (Interval x y) : f rest
                f [] = []

            let result = foldl (\values map_ -> concatMap (convert map_) values) seeds_ maps

            print result
            print (minimum (map (\Interval {..} -> start) result))
            

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop start))