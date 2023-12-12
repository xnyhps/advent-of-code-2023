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

data Tile = Operational | Damaged | Unknown
    deriving (Eq, Show, Ord)

parseLine = do
    tiles <- many1 (Operational <$ char '.' <|> Damaged <$ char '#' <|> Unknown <$ char '?')
    char ' '
    groups <- sepBy1 decimal (char ',')

    return (intercalate [Unknown] (replicate 5 tiles), concat (replicate 5 groups))

fileParser = sepBy1 parseLine (char '\n')

-- calculateGroups [] = []
-- calculateGroups (Damaged:xs) = (length (takeWhile (== Damaged) xs) + 1) : (calculateGroups (dropWhile (== Damaged) xs))
-- calculateGroups (Operational:xs) = calculateGroups xs

-- makeOptions [] = [[]]
-- makeOptions (Damaged:xs) = map (Damaged:) (makeOptions xs)
-- makeOptions (Operational:xs) = map (Operational:) (makeOptions xs)
-- makeOptions (Unknown:xs) = let rest = makeOptions xs in map (Damaged:) rest ++ map (Operational:) rest

-- analyze (tiles, groups) = let options = makeOptions tiles in filter (\x -> calculateGroups x == groups) options

runDamaged :: Int -> [Tile] -> [Int] -> DM.Map ([Tile], [Int]) Int -> (Int, DM.Map ([Tile], [Int]) Int)
runDamaged 0 (Damaged:xs) _ seen = (0, seen)
runDamaged i (Damaged:xs) groups seen = runDamaged (i - 1) xs groups seen
runDamaged 0 (Unknown:xs) groups seen = run xs groups seen
runDamaged i (Unknown:xs) groups seen = runDamaged (i - 1) xs groups seen
runDamaged 0 (Operational:xs) groups seen = run xs groups seen
runDamaged _ (Operational:xs) _ seen = (0, seen)
runDamaged 0 [] [] seen = (1, seen)
runDamaged _ [] _ seen = (0, seen)


run :: [Tile] -> [Int] -> DM.Map ([Tile], [Int]) Int -> (Int, DM.Map ([Tile], [Int]) Int)
run tiles groups seen | Just v <- DM.lookup (tiles, groups) seen = (v, seen)
run [] [] seen = (1, seen)
run [] _  seen = (0, seen)
run tiles@(Operational:xs) groups seen =
    let (v, seen_) = run xs groups seen
    in (v, DM.insert (tiles, groups) v seen_)
run tiles@(Damaged:xs) (i:groups) seen =
    let (v, seen_) = runDamaged (i - 1) xs groups seen
    in (v, DM.insert (tiles, (i:groups)) v seen_)
run (Damaged:xs) [] seen = (0, seen)
run (Unknown:xs) [] seen = run xs [] seen
run (Unknown:xs) (i:groups) seen =
    let
        (o, seen_) = run xs (i:groups) seen
        (d, seen__) = runDamaged (i - 1) xs groups (DM.insert (xs, (i:groups)) o seen_)
    in (o + d, seen__)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    startTime <- getCurrentTime

    print startTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            -- print values

            -- let result = map analyze values

            -- print (map length result)
            -- print (sum (map length result))

            let result = parMap rdeepseq (\(x,y) -> fst (run x y DM.empty)) values

            mapM_ print result
            print (sum result)

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))