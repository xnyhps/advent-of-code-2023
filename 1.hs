#!/usr/bin/env stack
{- stack
 runghc
 --package attoparsec
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text

import Control.Monad (void, forM_, when)
import Control.Applicative ((<|>))
import qualified Data.Text as DT
import qualified Data.Text.IO as DT
import Data.List (transpose, permutations, sort, isPrefixOf, isSuffixOf)
import qualified Data.Map.Lazy as DM
import Data.Char

import Debug.Trace

spellingMap = [("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)]

selectDigits :: String -> Integer
selectDigits str = (digit1 str) * 10 + digit2 str
    where
        digit1 :: String -> Integer
        digit1 str | isDigit (head str) = read [head str]
        digit1 str | [(_, d)] <- filter (\(name, _) -> name `isPrefixOf` str) spellingMap = d
        digit1 (s:str) = digit1 str
        
        digit2 :: String -> Integer
        digit2 str | isDigit (last str) = read [last str]
        digit2 str | [(_, d)] <- filter (\(name, _) -> name `isSuffixOf` str) spellingMap = d
        digit2 str = digit2 (reverse (tail (reverse str)))

main :: IO ()
main = do
    input <- DT.readFile "1.input"

    case parseOnly (sepBy1 (many1 (digit <|> letter)) (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values
            print (sum $ map selectDigits values)