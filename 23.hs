#!/usr/bin/env stack
{- stack
 runghc
 --package attoparsec
 --package filepath
 --package pseudomacros
 --package split
 --package PSQueue
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
import Data.Char (chr, ord, toLower, isDigit, isLower)
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

import qualified Data.PSQueue as PSQ
import Data.PSQueue(Binding(..))

data Tile = Empty | Wall | SlopeNorth | SlopeEast | SlopeSouth | SlopeWest
    deriving (Eq, Show)

fileParser = sepBy1 parseLine (char '\n')
    where
        parseTile = choice $ map (\(c,t) -> t <$ char c) [('.', Empty), ('#', Wall), ('^', SlopeNorth), ('v', SlopeSouth), ('>', SlopeEast), ('<', SlopeWest)]

        parseLine = many1 parseTile

directions = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx == 0 || dy == 0, dx /= 0 || dy /= 0]

findIntersections grid = filter (\(x,y) -> length (filter (f (x, y)) directions) > 2) $ DS.toList grid
    where
        f (x, y) (dx, dy) = DS.member (x + dx, y + dy) grid

followEdge :: Int -> (Int, Int) -> (Int, Int) -> DS.Set (Int, Int) -> [(Int,(Int, Int))] -> (Int, Int)
followEdge steps (x,y) (prevx, prevy) grid nodes | [(label,_)] <- filter (\(_,point) -> point == (x, y)) nodes = (label, steps)
followEdge steps (x,y) (prevx, prevy) grid nodes = let
    [newpoint] = filter (flip DS.member grid) $ filter (/= (prevx, prevy)) $ map (\(dx, dy) -> (x + dx, y + dy)) directions
    in followEdge (steps + 1) newpoint (x,y) grid nodes

makeGraph grid [] nodes = []
makeGraph grid ((label,(x,y)):xs) nodes = let
    outgoing = filter (flip DS.member grid) $ map (\(dx, dy) -> (x + dx, y + dy)) directions
    reachable = map (\edge -> let (to, steps) = followEdge 1 edge (x,y) grid nodes in ((label, to), steps)) outgoing
    in reachable ++ (makeGraph grid xs nodes)

findLongestPath :: [((Int, Int), Int)] -> Int -> Int -> Maybe Int
findLongestPath graph end node | end == node = Just 0
findLongestPath graph end node = let
    edges = filter (\((from, to), steps) -> from == node) graph
    newGraph = filter (\((from, to), steps) -> to /= node) graph
    in case catMaybes $ parMap rdeepseq (\((_,to),steps) -> fmap (steps + ) (findLongestPath newGraph end to)) edges of
        [] -> Nothing
        xs -> Just (maximum xs)

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
                grid :: DS.Set (Int, Int)
                grid = DS.fromList $ DM.keys $ DM.filter (/= Wall) $ DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) values
                width = length (values !! 0)
                height = length values
                [start] = DS.toList $ DS.filter (\(x,y) -> y == 0) grid
                [end] = DS.toList $ DS.filter (\(x,y) -> y == height - 1) grid

                nodes = zip [0..] (start : end : findIntersections grid)
                graph = makeGraph grid nodes nodes

                result = findLongestPath graph 1 0
            
            print nodes
            print graph
            print result


    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))
