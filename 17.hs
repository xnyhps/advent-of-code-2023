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

import qualified Data.PSQueue as PSQ
import Data.PSQueue(Binding(..))

data Direction = L | R | U | D
    deriving (Eq, Show, Ord)

fileParser = sepBy1 (many1 (read . (:[]) <$> digit)) (char '\n')

-- turnLeft L (x,y) = ((x,y+1), D, 1)
-- turnLeft R (x,y) = ((x,y-1), U, 1)
-- turnLeft D (x,y) = ((x+1,y), R, 1)
-- turnLeft U (x,y) = ((x-1,y), L, 1)

-- turnRight L (x,y) = ((x,y-1), U, 1)
-- turnRight R (x,y) = ((x,y+1), D, 1)
-- turnRight D (x,y) = ((x-1,y), L, 1)
-- turnRight U (x,y) = ((x+1,y), R, 1)

-- moveDirection (x,y) U = (x,y-1)
-- moveDirection (x,y) D = (x,y+1)
-- moveDirection (x,y) L = (x-1,y)
-- moveDirection (x,y) R = (x+1,y)

-- run :: DM.Map (Int, Int) Int -> (Int, Int) -> PSQ.PSQ ((Int, Int), Direction, Int) Int -> Int
-- run grid end queue | Just (((x,y),direction,steps) :-> value, newQueue) <- PSQ.minView queue, (x,y) == end = value
-- run grid end queue | Just (((x,y),direction,steps) :-> value, newQueue) <- PSQ.minView queue = let
--     left = let newLocation@((newx, newy),_,_) = turnLeft direction (x,y) in case DM.lookup (newx, newy) grid of 
--         Just tileValue -> PSQ.insertWith min newLocation (value + tileValue) newQueue
--         Nothing -> newQueue
    
--     right = let newLocation@((newx, newy),_,_) = turnRight direction (x,y) in case DM.lookup (newx, newy) grid of 
--         Just tileValue -> PSQ.insertWith min newLocation (value + tileValue) left
--         Nothing -> left
    
--     maybeStraight = if steps < 3 then let newLocation@(newx, newy) = moveDirection (x,y) direction in case DM.lookup (newx, newy) grid of
--             Just tileValue -> PSQ.insertWith min (moveDirection (x,y) direction,direction,steps + 1) (value + tileValue) right
--             Nothing -> right
--         else right

--     in run grid end maybeStraight

turnLeft L (x,y) steps = ((x,y+steps), D)
turnLeft R (x,y) steps = ((x,y-steps), U)
turnLeft D (x,y) steps = ((x+steps,y), R)
turnLeft U (x,y) steps = ((x-steps,y), L)

turnRight L (x,y) steps = ((x,y-steps), U)
turnRight R (x,y) steps = ((x,y+steps), D)
turnRight D (x,y) steps = ((x-steps,y), L)
turnRight U (x,y) steps = ((x+steps,y), R)

run :: DM.Map (Int, Int) Int -> (Int, Int) -> PSQ.PSQ ((Int, Int), Direction) Int -> Int
run grid end queue | Just (((x,y),direction) :-> value, newQueue) <- PSQ.minView queue, (x,y) == end = value
run grid end queue | Just (((x,y),direction) :-> value, newQueue) <- PSQ.minView queue = let

    move turnFunction queue steps = case sequence (map (\step -> let newLocation@((newx, newy), _) = turnFunction direction (x,y) step in DM.lookup (newx, newy) grid) [1..steps]) of
        Nothing -> queue
        Just scores -> let newLocation = turnFunction direction (x,y) steps in PSQ.insertWith min newLocation (value + sum scores) queue
    
    left = foldl' (move turnLeft) newQueue [4..10]

    right = foldl' (move turnRight) left [4..10]

    in run grid end right

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    startTime <- getCurrentTime

    print startTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            print values

            let
                height = length values
                width = length (values !! 0)

                grid = DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) values

                -- result = run grid (width - 1, height - 1) (PSQ.singleton ((0,0),U,0) 0)
                result = run grid (width - 1, height - 1) (PSQ.fromList (map (\direction -> ((((0,0),direction)) :-> 0)) [U,D,L,R]))

            print result


    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))