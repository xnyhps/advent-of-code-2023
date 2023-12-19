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
import Data.Char (chr, ord, toLower, isDigit, isLower)
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

data Result = Accept | Reject
    deriving (Show, Eq)

data Attribute = X | M | A | S
    deriving (Show, Eq)

data Check = LessThan | GreaterThan
    deriving (Show, Eq)

data Item = Item
    { itemX :: Int
    , itemM :: Int
    , itemA :: Int
    , itemS :: Int
    }
    deriving (Show, Eq)

data Rule = Rule
    { ruleAttribute :: Attribute
    , ruleCheck :: Check
    , ruleTarget :: Int
    , ruleTo :: Either Main.Result String
    }
    deriving (Show, Eq)

lower = satisfy isLower

parseRule = do
    attribute <- choice [X <$ char 'x', M <$ char 'm', A <$ char 'a', S <$ char 's']
    check <- (LessThan <$ char '<') <|> (GreaterThan <$ char '>')
    target <- decimal
    char ':'
    to <- parseTarget
    return (Rule attribute check target to)

parseTarget = (Left Accept <$ char 'A') <|> (Left Reject <$ char 'R') <|> (Right <$> many1 lower)

parseWorkflow = do
    name <- many1 lower
    char '{'
    rules <- sepBy1 parseRule (char ',')
    char ','
    finally <- parseTarget
    char '}'
    return (name, (rules, finally))

parseWorkflows = DM.fromList <$> sepBy1 parseWorkflow (char '\n')

parsePart = do
    string "{x="
    x <- decimal
    string ",m="
    m <- decimal
    string ",a="
    a <- decimal
    string ",s="
    s <- decimal
    char '}'

    return (Item x m a s)


parseParts = sepBy1 parsePart (char '\n')

fileParser = do
    workflows <- parseWorkflows
    string "\n\n"
    parts <- parseParts
    return (workflows, parts)


-- run workflows ([], Right ruleName) item = run workflows (DM.findWithDefault (error "Rule not found") ruleName workflows) item
-- run workflows ([], Left result) item = result
-- run workflows (Rule {..}:rules, finally) item@(Item {..}) = let
--     itemAttribute = case ruleAttribute of
--         X -> itemX
--         M -> itemM
--         A -> itemA
--         S -> itemS
--     check = case ruleCheck of
--         LessThan -> (<)
--         GreaterThan -> (>)
--     in if check itemAttribute ruleTarget
--         then case ruleTo of
--             Left result -> result
--             Right ruleName -> run workflows (DM.findWithDefault (error ("Rule not found: " ++ ruleName)) ruleName workflows) item
--         else run workflows (rules, finally) item

splitRanges X ruleTarget range@((minX, maxX), m, a, s) | ruleTarget >= minX, ruleTarget < maxX = (Just ((minX, ruleTarget), m, a, s), Just ((ruleTarget + 1, maxX), m, a, s))
splitRanges X ruleTarget range@((minX, maxX), m, a, s) | ruleTarget > maxX = (Nothing, Just range)
splitRanges X ruleTarget range@((minX, maxX), m, a, s) | ruleTarget < minX = (Just range, Nothing)

splitRanges M ruleTarget range@(x, (minM, maxM), a, s) | ruleTarget >= minM, ruleTarget < maxM = (Just (x, (minM, ruleTarget), a, s), Just (x, (ruleTarget + 1, maxM), a, s))
splitRanges M ruleTarget range@(x, (minM, maxM), a, s) | ruleTarget > maxM = (Nothing, Just range)
splitRanges M ruleTarget range@(x, (minM, maxM), a, s) | ruleTarget < minM = (Just range, Nothing)

splitRanges A ruleTarget range@(x, m, (minA, maxA), s) | ruleTarget >= minA, ruleTarget < maxA = (Just (x, m, (minA, ruleTarget), s), Just (x, m, (ruleTarget + 1, maxA), s))
splitRanges A ruleTarget range@(x, m, (minA, maxA), s) | ruleTarget > maxA = (Nothing, Just range)
splitRanges A ruleTarget range@(x, m, (minA, maxA), s) | ruleTarget < minA = (Just range, Nothing)

splitRanges S ruleTarget range@(x, m, a, (minS, maxS)) | ruleTarget >= minS, ruleTarget < maxS = (Just (x, m, a, (minS, ruleTarget)), Just (x, m, a, (ruleTarget + 1, maxS)))
splitRanges S ruleTarget range@(x, m, a, (minS, maxS)) | ruleTarget > maxS = (Nothing, Just range)
splitRanges S ruleTarget range@(x, m, a, (minS, maxS)) | ruleTarget < minS = (Just range, Nothing)


run workflows ([], Right ruleName) item = run workflows (DM.findWithDefault (error "Rule not found") ruleName workflows) item
run workflows ([], Left Accept) itemRanges = [itemRanges]
run workflows ([], Left Reject) itemRanges = []
run workflows (Rule {..}:rules, finally) itemRanges = let
    (lower, upper) = splitRanges ruleAttribute (case ruleCheck of
        LessThan -> ruleTarget - 1
        GreaterThan -> ruleTarget) itemRanges

    (match, nonMatch) = case ruleCheck of
        LessThan -> (lower, upper)
        GreaterThan -> (upper, lower)

    matchResult = case (match, ruleTo) of
        (Nothing, _) -> []
        (Just range, Right ruleName) -> run workflows (DM.findWithDefault (error "Not found") ruleName workflows) range
        (Just range, Left Accept) -> [range]
        (Just range, Left Reject) -> []

    nonMatchResult = case nonMatch of
        Nothing -> []
        Just range -> run workflows (rules, finally) range

    in matchResult ++ nonMatchResult

getSize ((minX, maxX), (minM, maxM), (minA, maxA), (minS, maxS)) = (maxX + 1 - minX) * (maxM + 1 - minM) * (maxA + 1 - minA) * (maxS + 1 - minS)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    startTime <- getCurrentTime

    print startTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values@(workflows, parts) -> do
            print values

            -- let result = sum $ map (\Item{..} -> itemX + itemM + itemA + itemS) $ filter (\part -> run workflows (DM.findWithDefault (error "Impossible") "in" workflows) part == Accept) parts

            -- print result

            let result = run workflows (DM.findWithDefault (error "Impossible") "in" workflows) ((1, 4000), (1, 4000), (1, 4000), (1, 4000))

            print result

            print (map getSize result)
            print (sum (map getSize result))

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))