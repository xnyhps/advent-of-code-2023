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

fileParser = sepBy1 (sepBy1 (signed decimal) (char ' ')) (char '\n')

derivative xs = zipWith (-) (drop 1 xs) xs

calculateNext xs | all (== 0) xs = 0
calculateNext xs = last xs + calculateNext (derivative xs)

calculatePrevious xs | all (== 0) xs = 0
calculatePrevious xs = head xs - calculatePrevious (derivative xs)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    start <- getCurrentTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            print values

            -- print (map calculateNext values)

            -- print (sum (map calculateNext values))

            print (map calculatePrevious values)

            print (sum (map calculatePrevious values))

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop start))