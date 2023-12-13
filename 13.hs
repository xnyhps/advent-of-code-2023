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
import qualified Data.Map.Strict as DM
import qualified Data.Set as DS
import Data.Ord (comparing)
import Data.Char (chr, ord, toLower, isDigit)
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Traversable (for)
import Control.Parallel.Strategies (parMap, rdeepseq)

import Data.Time

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace
import GHC.Stack (HasCallStack)

data Tile = Ash | Rock
    deriving (Eq, Show, Ord)

parseMap = sepBy1 (many1 (Ash <$ char '.' <|> Rock <$ char '#')) (char '\n')

fileParser = sepBy1 parseMap (string "\n\n")

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    startTime <- getCurrentTime

    print startTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            let scores = (flip map) values $ \value -> let
                            width = length (value !! 0)
                            height = length value
                            grid = DS.fromList $ DM.keys $ DM.filter (== Rock) $ DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) value

                            -- horizontalReflections = [column | column <- [1..width-1], let filtered = DS.filter (\(x,y) -> x < 2 * column && x >= (column - (width - column))) grid, let mirrored = DS.map (\(x,y) -> (2 * column - 1 - x, y)) filtered, filtered == mirrored]

                            -- verticalReflections = [row | row <- [1..height-1], let filtered = DS.filter (\(x,y) -> y < 2 * row && y >= (row - (height - row))) grid, let mirrored = DS.map (\(x,y) -> (x, 2 * row - 1 - y)) filtered, filtered == mirrored]

                            horizontalReflections = [column | column <- [1..width-1], let filtered = DS.filter (\(x,y) -> x < 2 * column && x >= (column - (width - column))) grid, let mirrored = DS.map (\(x,y) -> (2 * column - 1 - x, y)) filtered, DS.size ((DS.union filtered mirrored) `DS.difference` (DS.intersection filtered mirrored)) == 2]

                            verticalReflections = [row | row <- [1..height-1], let filtered = DS.filter (\(x,y) -> y < 2 * row && y >= (row - (height - row))) grid, let mirrored = DS.map (\(x,y) -> (x, 2 * row - 1 - y)) filtered, DS.size ((DS.union filtered mirrored) `DS.difference` (DS.intersection filtered mirrored)) == 2]

                        in case (horizontalReflections, verticalReflections) of
                            ([x], []) -> x
                            ([], [y]) -> 100 * y
                            x -> error (show x)

            print (sum scores)


    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))