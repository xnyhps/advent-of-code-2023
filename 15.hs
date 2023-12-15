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
import Data.Word
import Data.Maybe
import Data.Traversable (for)
import Control.Parallel.Strategies (parMap, rdeepseq)

import Data.Time

import System.FilePath (replaceExtension)

import PseudoMacros

import Debug.Trace
import GHC.Stack (HasCallStack)

data Operation = Remove String | Replace String Int
    deriving (Show)

parseOperation = (do
    label <- many1 letter
    char '-'
    return (Remove label)) <|> (do
        label <- many1 letter
        char '='
        value <- decimal
        return (Replace label value)
    )

fileParser = sepBy1 parseOperation (char ',')

hash value = foldl (\acc c -> (17 * (acc + (fromIntegral (ord c)))) `mod` 256) 0 value

splitFirst predicate list = let (prefix, suffix) = break predicate list in case suffix of
    [] -> Nothing
    (x:xs) -> Just (prefix,x,xs)

applyHashMap :: DM.Map Int [(String, Int)] -> Operation -> DM.Map Int [(String, Int)]
applyHashMap boxes (Remove label) | Just box <- DM.lookup (hash label) boxes = DM.update (Just . filter (\(l, _) -> l /= label)) (hash label) boxes
applyHashMap boxes (Replace label value) | Just box <- DM.lookup (hash label) boxes = case splitFirst (\(l, _) -> l == label) box of
    Just (prefix,_,suffix) -> DM.insert (hash label) (prefix ++ [(label, value)] ++ suffix) boxes
    Nothing -> DM.update (\x -> Just (x ++ [(label, value)])) (hash label) boxes

calculateScore :: DM.Map Int [(String, Int)] -> Int
calculateScore boxes = sum $ map (\(box, lenses) -> sum (map (\(i, (lens, value)) -> (box + 1) * i * value) (zip [1..] lenses))) $ DM.toList boxes

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    startTime <- getCurrentTime

    print startTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            -- print values

            -- print (map hash values)
            -- print (sum (map hash values))

            let result = foldl' applyHashMap (DM.fromList [(x, []) | x <- [0..255]]) values

            -- print result
            print (calculateScore result)

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))