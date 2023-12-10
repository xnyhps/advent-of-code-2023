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

data Tile = Ground | Start | NS | EW | NE | NW | SW | SE
    deriving (Show, Eq)

directions =
    [ (NS, ((0, -1), (0, 1)))
    , (EW, ((1, 0), (-1, 0)))
    , (NE, ((1, 0), (0, -1)))
    , (NW, ((-1, 0), (0, -1)))
    , (SW, ((-1, 0), (0, 1)))
    , (SE, ((1, 0), (0, 1)))
    ]

getRotation (0, dyIn) (0, dyOut) = 0
getRotation (dxIn, 0) (dxOut, 0) = 0
getRotation (1, 0) (0, -1) = 90 -- E -> N
getRotation (-1, 0) (0, -1) = -90 -- W -> N
getRotation (-1, 0) (0, 1) = 90 -- W -> S
getRotation (1, 0) (0, 1) = -90 -- E -> S
getRotation (dxIn, dyIn) (dxOut, dyOut) = - (getRotation (dxOut, dyOut) (dxIn, dyIn))

getInside (0, -1) (0, 1) = [(-1, 1), (-1, 0), (-1, -1)] -- N -> S
getInside (0, 1) (0, -1) = [(1, 1), (1, 0), (1, -1)] -- S -> N
getInside (1, 0) (-1, 0) = [(1, -1), (0, -1), (-1, -1)] -- E -> W
getInside (-1, 0) (1, 0) = [(1, 1), (0, 1), (-1, 1)] -- W -> E

getInside (0, 1) (1, 0) = [(1, 1)] -- S -> E
getInside (-1, 0) (0, 1) = [(-1, 1)] -- W -> S
getInside (0, -1) (-1, 0) = [(-1, -1)] -- N -> W
getInside (1, 0) (0, -1) = [(1, -1)] -- E -> N

getInside (0, 1) (-1, 0) = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1)] -- S -> W
getInside (-1, 0) (0, -1) = [(1, -1), (1, 0), (1, 1), (0, 1), (-1, 1)] -- W -> N
getInside (0, -1) (1, 0) = [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1)] -- N -> E
getInside (1, 0) (0, 1) = [(1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)] -- E -> S

parseTile = choice (map (\(c,tile) -> tile <$ char c) [('.', Ground), ('S', Start), ('|', NS), ('-', EW), ('L', NE), ('J', NW), ('7', SW), ('F', SE)])

fileParser = sepBy1 (many1 parseTile) (char '\n')

-- go location start _ grid distance | location == start = distance
-- go location start previous grid distance = case DM.lookup location grid of
--     Just Ground -> error "Impossible"
--     Just Start -> error "Impossible"
--     Nothing -> error "Impossible"
--     Just tile -> case lookup tile directions of
--         Just (a, b) | applyDelta a location == previous -> go (applyDelta b location) start location grid (distance + 1)
--         Just (a, b) | applyDelta b location == previous -> go (applyDelta a location) start location grid (distance + 1)

go location start _ _ | location == start = ([], 0)
go location start previous grid = case DM.lookup location grid of
    Just Ground -> error ("Impossible: " ++ show location)
    Just Start -> error ("Impossible: " ++ show location)
    Nothing -> error ("Impossible: " ++ show location)
    Just tile -> let
            (o, i) = case lookup tile directions of
                Just (a, b) | applyDelta a location == previous -> (b, a)
                Just (a, b) | applyDelta b location == previous -> (a, b)
            (seen, rotation) = go (applyDelta o location) start location grid
        in ((location, i, o):seen, rotation + getRotation i o)

go2 [] = DS.empty
go2 ((location,i,o):points) = let
    inside = getInside i o
    in DS.fromList (map (\x -> applyDelta x location) inside) `DS.union` go2 points 

applyDelta (dx, dy) (x, y) = (x + dx, y + dy)

growInternal inside outside =
    let
        neighbours = concatMap getNeighbours (DS.toList inside)
        getNeighbours (x, y) = filter (`DS.notMember` inside) $ filter (`DS.notMember` outside) [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0, dx == 0 || dy == 0]
    in case neighbours of
        [] -> inside
        _ -> growInternal (DS.union (DS.fromList neighbours) inside) outside

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    start <- getCurrentTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            -- print values

            let
                grid = DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) values
                [(start@(startx, starty), _)] = DM.toList $ DM.filter (== Start) grid
                next = filter matchesDirection [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0, dx == 0 || dy == 0]

                matchesDirection (dx, dy) = case DM.lookup (startx + dx, starty + dy) grid of
                    Just tile -> case lookup tile directions of
                        Just (a,b) | a == (-dx, -dy) || b == (-dx, -dy) -> True
                        _ -> False
                    _ -> False

                location = applyDelta (head next) start

                (visited, rotation) = go location start start grid

                order = if rotation < 0 then map (\(a,b,c) -> (a,c,b)) (reverse visited) else visited

                seen = DS.insert start (DS.fromList (map (\(x, _, _) -> x) visited))

                inside = go2 order `DS.difference` seen

                result = growInternal inside seen

            -- print visited
            print result
            print (DS.size result)

            
    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop start))