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
import Control.Applicative ((<|>))
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import Data.List
import qualified Data.Map.Lazy as DM
import qualified Data.Set as DS
import Data.Ord (comparing)
import Data.Char (chr, ord, toLower)
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Traversable (for)

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

data Color = R | G | B
    deriving (Eq, Show, Ord)

data Game = Game {
    roundNumber :: Int,
    observations :: [DM.Map Color Int]
    } deriving (Show)

parseResult = DM.fromListWith undefined <$> sepBy1 (do
    count <- decimal
    char ' '
    color <- (R <$ string "red") <|> (G <$ string "green") <|> (B <$ string "blue")
    return (color, count))
        (string ", ")

parseLine = do
    string "Game "
    roundNumber <- decimal
    string ": "
    observations <- sepBy1 parseResult (string "; ")

    return (Game roundNumber observations)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            -- let
            --     isPossible observation = maybe True (<= 12) (DM.lookup R observation)
            --         && maybe True (<= 13) (DM.lookup G observation)
            --         && maybe True (<= 14) (DM.lookup B observation)
            --     possible = filter (\(Game {..}) -> all isPossible observations) values

            -- print $ sum $ map roundNumber possible

            let powers = map (\(Game {..}) -> (\(r,g,b) -> r * g * b) $ foldr findMaximum (0, 0, 0) observations) values
                findMaximum observation (r,g,b) = (f r R, f g G, f b B)
                    where
                        f value color = maybe value (max value) (DM.lookup color observation)

            print powers
            print (sum powers)