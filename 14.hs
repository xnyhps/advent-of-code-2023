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

data Tile = Empty | Cube | Sphere
    deriving (Eq, Ord)

instance Show Tile where
    show Empty = "."
    show Cube = "#"
    show Sphere = "O"

fileParser = sepBy1 (many1 (Empty <$ char '.' <|> Cube <$ char '#' <|> Sphere <$ char 'O')) (char '\n')

-- tryRolling :: (Int, Int) -> DM.Map (Int, Int) Tile -> DM.Map (Int, Int) Tile
-- tryRolling (x,y) grid | Just Empty <- DM.lookup (x,y) grid = grid
-- tryRolling (x,y) grid | Just Cube <- DM.lookup (x,y) grid = grid
-- tryRolling (x,y) grid | Just Sphere <- DM.lookup (x,y) grid = case takeWhile (\newy -> DM.lookup (x,newy) grid == Just Empty) [y-1,y-2..0] of
--     newy@(_:_) -> DM.insert (x,last newy) Sphere (DM.insert (x,y) Empty grid)
--     [] -> grid
-- tryRolling (x,y) grid = error (show (x,y))

tryRolling :: (Int, Int) -> (Int, Int) -> DM.Map (Int, Int) Tile -> DM.Map (Int, Int) Tile
tryRolling (dx,dy) (x,y) grid = case DM.lookup (x,y) grid of
    Just Empty -> grid
    Just Cube -> grid
    Just Sphere -> case takeWhile (\(newx, newy) -> DM.lookup (newx,newy) grid == Just Empty) [(x + n * dx, y + n * dy) | n <- [1..]] of
        new@(_:_) -> DM.insert (last new) Sphere (DM.insert (x,y) Empty grid)
        [] -> grid

printGrid width height grid = concat [concat [show (DM.findWithDefault (error "Hole") (x,y) grid) | x <- [0..width-1]] ++ "\n" | y <- [0..height-1]]

calculateLoad height grid = sum (map (\(_,y) -> height - y) (DM.keys (DM.filter (== Sphere) grid)))

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

                rollNorth grid = foldl' (flip (tryRolling (0, -1))) grid [(x,y) | x <- [0..width-1], y <- [0..height-1]]

                rollSouth grid = foldl' (flip (tryRolling (0, 1))) grid [(x,y) | x <- [0..width-1], y <- [height-1,height-2..0]]

                rollWest grid = foldl' (flip (tryRolling (-1, 0))) grid [(x,y) | y <- [0..height-1], x <- [0..width-1]]

                rollEast grid = foldl' (flip (tryRolling (1, 0))) grid [(x,y) | y <- [0..height-1], x <- [width-1,width-2..0]]

                run grid 0 seen = grid
                run grid n seen | Just previous <- DM.lookup grid seen = let cycleLength = previous - n in run grid (n `mod` cycleLength) DM.empty
                run grid n seen = let newgrid = rollEast (rollSouth (rollWest (rollNorth grid))) in run newgrid (n - 1) (DM.insert grid n seen)

                result = run grid 1000000000 DM.empty

            -- print grid
            -- putStrLn (printGrid width height result)
            print (calculateLoad height result)

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))