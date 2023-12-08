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
import GHC.Stack (HasCallStack)

data Direction = L | R
    deriving (Show, Eq)

parseLine = do
    node <- count 3 (letter <|> digit)
    string " = ("
    lnode <- count 3 (letter <|> digit)
    string ", "
    rnode <- count 3 (letter <|> digit)
    char ')'

    return (node, (lnode, rnode))

fileParser = do
    directions <- many1 (L <$ char 'L' <|> R <$ char 'R')

    string "\n\n"

    network <- sepBy1 parseLine (char '\n')

    return (directions, DM.fromList network)

-- run "ZZZ" _ _ = 0
-- run node network (L:directions) | Just (lnode, _) <- DM.lookup node network = 1 + run lnode network directions
-- run node network (R:directions) | Just (_, rnode) <- DM.lookup node network = 1 + run rnode network directions
-- run node network _ = error ("Node " ++ show node ++ " not found in " ++ show network)

-- Not necessary: all sequences have matches on 0 mod period...
findLowestCommonElement :: [[Int]] -> Int
findLowestCommonElement lists =
    let
        heads = map head lists
        smallest = minimum heads
        highest = maximum heads
    in if smallest == highest then smallest
        else  findLowestCommonElement (map (dropWhile (< highest)) lists)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    start <- getCurrentTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values@(directions, network) -> do
            print values

            -- let result = run "AAA" network (cycle directions)

            -- print result

            let
                startNodes = filter (\name -> last name == 'A') (DM.keys network)

                directionsCycleLength = length directions

                findPeriod directions loop seen zs node | Just earlierLoop <- DM.lookup (node, loop `mod` directionsCycleLength) seen = (earlierLoop, loop - earlierLoop, map (\z -> z - earlierLoop) $ reverse zs)
                findPeriod directions loop seen zs node | Just (lnode, rnode) <- DM.lookup node network = 
                    let nextNode = case (directions !! (loop `mod` directionsCycleLength)) of
                            L -> lnode
                            R -> rnode
                    in findPeriod directions (loop + 1) (DM.insert (node, loop `mod` directionsCycleLength) loop seen) (if last node == 'Z' then loop:zs else zs) nextNode
                periods = map (findPeriod directions 0 DM.empty []) startNodes
                generateSequence (start, cycleLength, points) = [start + i * cycleLength + point | i <- [0..], point <- points]

            print startNodes

            print periods

            print (foldr lcm 1 $ map (\(a,b,c) -> b) periods)

            -- print (findLowestCommonElement $ map generateSequence periods)

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop start))