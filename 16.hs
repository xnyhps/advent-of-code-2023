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
import Data.Word
import Data.Maybe
import Data.Traversable (for)
import Control.Parallel.Strategies (parMap, rdeepseq)

import Data.Time

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace
import GHC.Stack (HasCallStack)

data Tile = Empty | MirrorRightUp | MirrorLeftUp | VerticalSplitter | HorizontalSplitter
    deriving (Show, Eq)

data Direction = L | R | U | D
    deriving (Show, Eq, Ord)

parseTile = Empty <$ char '.' <|> MirrorRightUp <$ char '/' <|> MirrorLeftUp <$ char '\\' <|> VerticalSplitter <$ char '|' <|> HorizontalSplitter <$ char '-'

fileParser = sepBy1 (many1 parseTile) (char '\n')

moveDirection R (x,y) = (x + 1, y)
moveDirection L (x,y) = (x - 1, y)
moveDirection U (x,y) = (x, y - 1)
moveDirection D (x,y) = (x, y + 1)

run direction (x,y) grid visited | (direction,(x,y)) `DS.member` visited = visited
run direction (x,y) grid visited = case DM.lookup (x,y) grid of
    Nothing -> visited
    Just Empty -> run direction (moveDirection direction (x,y)) grid (DS.insert (direction,(x,y)) visited)
    Just MirrorRightUp -> let
            newDirection = case direction of
                R -> U
                U -> R
                L -> D
                D -> L
        in run newDirection (moveDirection newDirection (x,y)) grid (DS.insert (direction,(x,y)) visited)
    Just MirrorLeftUp -> let
            newDirection = case direction of
                L -> U
                U -> L
                R -> D
                D -> R
        in run newDirection (moveDirection newDirection (x,y)) grid (DS.insert (direction,(x,y)) visited)
    Just VerticalSplitter -> case direction of
        U -> run direction (moveDirection direction (x,y)) grid (DS.insert (direction,(x,y)) visited)
        D -> run direction (moveDirection direction (x,y)) grid (DS.insert (direction,(x,y)) visited)
        _ -> let
                up = run U (moveDirection U (x,y)) grid (DS.insert (direction,(x,y)) visited)
                down = run D (moveDirection D (x,y)) grid (DS.insert (direction,(x,y)) up)
            in down
    Just HorizontalSplitter -> case direction of
        L -> run direction (moveDirection direction (x,y)) grid (DS.insert (direction,(x,y)) visited)
        R -> run direction (moveDirection direction (x,y)) grid (DS.insert (direction,(x,y)) visited)
        _ -> let
                left = run L (moveDirection L (x,y)) grid (DS.insert (direction,(x,y)) visited)
                right = run R (moveDirection R (x,y)) grid (DS.insert (direction,(x,y)) left)
            in right

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    startTime <- getCurrentTime

    print startTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            -- print values

            let
                height = length values
                width = length (values !! 0)

                grid = DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) values

            --     result = run R (0, 0) grid DS.empty

            -- print result
            -- print (DS.size (DS.map snd result))

            let
                score = DS.size . DS.map snd
                left = [score (run R (0, y) grid DS.empty) | y <- [0..height-1]]
                right = [score (run L (width - 1, y) grid DS.empty) | y <- [0..height-1]]

                up = [score (run D (x, 0) grid DS.empty) | x <- [0..width-1]]
                down = [score (run U (x, height - 1) grid DS.empty) | x <- [0..width-1]]

            print (maximum (left ++ right ++ up ++ down))


    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))