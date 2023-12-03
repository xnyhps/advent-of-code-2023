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

parseDigit = digit

parseSpace = char '.'

parseSymbol = char '*' <|> char '#' <|> char '$' <|> char '+' <|> char '&' <|> char '%' <|> char '-' <|> char '@' <|> char '=' <|> char '/'

parseLine = many1 (parseDigit <|> parseSpace <|> parseSymbol)

isValid Nothing = True
isValid (Just chr) = not (isDigit chr)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    case parseOnly (sepBy1 parseLine (char '\n')) input of
        Left err -> error err
        Right values -> do
            print values

            let grid = DM.fromList $ concat $ zipWith (\row -> map (\(column,value) -> ((column, row), value))) [0..] $ map (zip [0..]) values

            print grid

            -- let partNumbers = (flip map) [0..length values - 1] $ \j ->
            --         (flip map) [0..length (values !! 0) - 1] $ \i ->
            --             case DM.lookup (i,j) grid of
            --                 Just chr | isDigit chr -> case DM.lookup (i - 1, j) grid of
            --                     chr_ | isValid chr_ ->
            --                         let
            --                             (value, width) = go i j grid 0
            --                             go x y grid value = case DM.lookup (x, y) grid of
            --                                 Just chr_ | isDigit chr_ -> let (newValue, width) = go (x + 1) y grid (value * 10 + read [chr_]) in (newValue, width + 1)
            --                                 _ -> (value, 0)
            --                             symbols = catMaybes $ concat $ (flip map) [-1..width] $ \di ->
            --                                 (flip map) [-1..1] $ \dj ->
            --                                     case DM.lookup (i + di, j + dj) grid of
            --                                         Just chr | chr /= '.' && not (isDigit chr) -> Just (di, dj)
            --                                         _ -> Nothing

            --                         in case symbols of
            --                             [] -> Nothing
            --                             _ -> Just value
            --                     _ -> Nothing
            --                 _ -> Nothing

            -- print (catMaybes $ concat partNumbers)
            -- print (sum $ catMaybes $ concat partNumbers)

            let gears = concat $ catMaybes $ concat $ (flip map) [0..length values - 1] $ \j ->
                    (flip map) [0..length (values !! 0) - 1] $ \i ->
                        case DM.lookup (i,j) grid of
                            Just chr | isDigit chr -> case DM.lookup (i - 1, j) grid of
                                chr_ | isValid chr_ ->
                                    let
                                        (value, width) = go i j grid 0
                                        go x y grid value = case DM.lookup (x, y) grid of
                                            Just chr_ | isDigit chr_ -> let (newValue, width) = go (x + 1) y grid (value * 10 + read [chr_]) in (newValue, width + 1)
                                            _ -> (value, 0)
                                        symbols = catMaybes $ concat $ (flip map) [-1..width] $ \di ->
                                            (flip map) [-1..1] $ \dj ->
                                                case DM.lookup (i + di, j + dj) grid of
                                                    Just '*' -> Just ((i + di, j + dj), value)
                                                    _ -> Nothing

                                    in Just symbols
                                _ -> Nothing
                            _ -> Nothing

            print gears

            let gearRatios = DM.fromListWith (*) $ filter (\(x, _) -> length (filter (\(y,_) -> x == y) gears) == 2) gears

            print gearRatios
            print (sum $ DM.elems gearRatios)