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

-- data Card = CA | CK | CQ | CJ | CT | C9 | C8 | C7 | C6 | C5 | C4 | C3 | C2
--     deriving (Show, Eq, Ord)

data Card = CA | CK | CQ | CT | C9 | C8 | C7 | C6 | C5 | C4 | C3 | C2 | CJ
    deriving (Show, Eq, Ord)

data Hand = Hand Card Card Card Card Card
    deriving (Show, Eq)

data HandType = FiveOfAKind | FourOfAKind | FullHhouse | ThreeOfAKind | TwoPair | OnePair | HighCard
    deriving (Show, Eq, Ord)

instance Ord Hand where
    compare l r | getType l /= getType r = compare (getType l) (getType r)
    compare (Hand la lb lc ld le) (Hand ra rb rc rd re) = compare (la, lb, lc, ld, le) (ra, rb, rc, rd, re)

counts :: [Card] -> [(Card, Int)]
counts xs = DM.toList (DM.fromListWith (+) (zip xs (repeat 1)))

-- getType :: Hand -> HandType
-- getType (Hand a b c d e) = let cardCounts = counts [a,b,c,d,e]
--     in case reverse (sort (map snd cardCounts)) of
--         [5] -> FiveOfAKind
--         (4:_) -> FourOfAKind
--         [3, 2] -> FullHhouse
--         (3:_) -> ThreeOfAKind
--         (2:2:_) -> TwoPair
--         (2:_) -> OnePair
--         _ -> HighCard

makeJokers 0 = [[]]
makeJokers n = let rest = makeJokers (n - 1) in [y : x | x <- rest, y <- [CA, CK, CQ, CT, C9, C8, C7, C6, C5, C4, C3, C2]]

getType :: Hand -> HandType
getType (Hand a b c d e) =
    let jokerCount = length (filter (== CJ) [a, b, c, d, e])
        rest = filter (/= CJ) [a, b, c, d, e]
        potentialTypes = map getType_ $ map (rest ++) (makeJokers jokerCount)
    in case jokerCount of
        5 -> FiveOfAKind
        4 -> FiveOfAKind
        _ -> minimum potentialTypes

getType_ :: [Card] -> HandType
getType_ cards = let cardCounts = counts cards
    in case reverse (sort (map snd cardCounts)) of
        [5] -> FiveOfAKind
        (4:_) -> FourOfAKind
        [3, 2] -> FullHhouse
        (3:_) -> ThreeOfAKind
        (2:2:_) -> TwoPair
        (2:_) -> OnePair
        _ -> HighCard

fileParser :: Parser [(Hand, Integer)]
fileParser = sepBy1 parseLine (char '\n')

parseCard =  try (choice (map (\(card, c) -> card <$ char c) [(CA, 'A'), (CK, 'K'), (CQ, 'Q'), (CJ, 'J'), (CT, 'T'), (C9, '9'), (C8, '8'), (C7, '7'), (C6, '6'), (C5, '5'), (C4, '4'), (C3, '3'), (C2, '2')]))

parseLine = do
    hand <- (\[a,b,c,d,e] -> Hand a b c d e) <$> count 5 parseCard
    space
    bid <- decimal <?> "Bid"
    return (hand, bid)


main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    start <- getCurrentTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            print values

            let result = zipWith (\i (_,bid) -> i * bid) [1..] (reverse (sort values))

            print (sum result)

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop start))