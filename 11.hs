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

import Data.Time

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace
import GHC.Stack (HasCallStack)

data Tile = Empty | Galaxy
    deriving (Eq, Show)

fileParser = sepBy1 (many1 (Empty <$ char '.' <|> Galaxy <$ char '#')) (char '\n')

manhattanDistance (a, b) (c, d) = abs (a - c) + abs (b - d)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    startTime <- getCurrentTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            -- print values

            let grid = DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) values

            let
                sizeIncrease = 1000000

                emptyRows = [y | y <- [0..length values - 1], all (\x -> DM.findWithDefault (error ("Impossible: " ++ show (x,y))) (x,y) grid == Empty) [0..length (values !! 0) - 1]]
                emptyColumns = [x | x <- [0..length (values !! 0) - 1], all (\y -> DM.findWithDefault (error ("Impossible: " ++ show (x,y))) (x,y) grid == Empty) [0..length values - 1]]

                fixedGrid = filter (\(_,value) -> value == Galaxy) $ concat $ zipWith (\row -> map (\(column,value) -> ((column + (sizeIncrease - 1) * length (filter (< column) emptyColumns), row + (sizeIncrease - 1) * length (filter (< row) emptyRows)), value))) [0..] $ map (zip [0..]) values

                distances = [manhattanDistance (fst from) (fst to) | from <- fixedGrid, to <- fixedGrid]

            -- print emptyRows
            -- print emptyColumns
            -- print fixedGrid
            -- print distances
            -- print (sum distances)
            print (sum distances `div` 2)

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))