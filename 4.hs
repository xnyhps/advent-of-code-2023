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

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace

parseLine = do
    string "Card"
    many1 space
    number <- decimal
    string ":"
    many1 space
    winningNumbers <- sepBy1 decimal (many1 space)
    many1 space
    char '|'
    many1 space
    myNumbers <- sepBy1 decimal (many1 space)
    return (number, winningNumbers, myNumbers)

scoreLength 0 = 0
scoreLength n = 2 ^ (n - 1)


main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            -- let points = map (\(_,winningNumbers,myNumbers) -> scoreLength $ length $ filter (`elem` winningNumbers) myNumbers) values

            -- print (sum points)

            let initialCards = map (\(number,_,_) -> number) values
                cardsAdded = DM.fromList (map (\number -> (number, f number)) initialCards)
                f number = let
                    (_,winningNumbers,myNumbers) = values !! (number - 1)
                    numberOfCardsWon = length $ (filter (`elem` winningNumbers)) myNumbers
                    rangeOfCardsWon = [number + i | i <- [1..numberOfCardsWon]]
                    in 1 + sum (map (\i -> DM.findWithDefault (error "Not found") i cardsAdded) rangeOfCardsWon)

            print initialCards
            print cardsAdded
            print (sum (DM.elems cardsAdded))