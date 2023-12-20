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

data ModuleType = FlipFlop | Conjunction
    deriving (Eq, Show)

data FlipFlopState = On | Off
    deriving (Eq, Show)

data ModuleState = FlipFlopState FlipFlopState | ConjunctionState (DM.Map String Signal)
    deriving (Eq, Show)

data Signal = Low | High
    deriving (Eq, Show)

invert On = Off
invert Off = On

toPulse On = High
toPulse Off = Low

data Pulse = Pulse {
    pulseSignal :: Signal,
    pulseFrom :: String,
    pulseTo :: String
    }
    deriving (Eq, Show)

parseLine = do
    moduleType <- option Nothing (Just <$> (FlipFlop <$ char '%' <|> Conjunction <$ char '&'))
    name <- many1 letter
    string " -> "
    outputs <- sepBy1 (many1 letter) (string ", ")
    return (name, (moduleType, outputs))

fileParser = DM.fromList <$> sepBy1 parseLine (char '\n')

run :: Pulse -> DM.Map String (Maybe ModuleType, [String]) -> DM.Map String ModuleState -> ([Pulse], Maybe (String, ModuleState))
run (Pulse signal _ name@"broadcaster") machine state | Just (_, outputs) <- DM.lookup name machine = (map (Pulse signal name) outputs, Nothing)
run (Pulse Low _ name) machine state
    | Just (Just FlipFlop, outputs) <- DM.lookup name machine
    , Just (FlipFlopState oldSignal) <- DM.lookup name state
    = (map (Pulse (toPulse (invert oldSignal)) name) outputs, Just (name, FlipFlopState (invert oldSignal)))
run (Pulse High _ name) machine state
    | Just (Just FlipFlop, outputs) <- DM.lookup name machine
    = ([], Nothing)
run (Pulse signal from name) machine state
    | Just (Just Conjunction, outputs) <- DM.lookup name machine
    , Just (ConjunctionState memory) <- DM.lookup name state
    = let
    newMemory = DM.insert from signal memory
    pulses = if all (== High) (DM.elems newMemory)
        then map (Pulse Low name) outputs
        else map (Pulse High name) outputs
    in (pulses, Just (name, ConjunctionState newMemory))
run (Pulse signal from name) machine state
    | Nothing <- DM.lookup name machine
    = ([], Nothing)
run x machine state = error (show x)

-- go 0 _ _ _ = (0, 0)
-- go n [] machine state = go (n - 1) [Pulse Low "button" "broadcaster"] machine state
-- go n pulses machine state = 
--     let
--         results = map (\pulse -> run pulse machine state) pulses
--         newState = foldr (\(name, moduleState) -> DM.insert name moduleState) state $ catMaybes $ map snd results
--         newPulses = concatMap fst results
--         (low, high) = go n newPulses machine newState
--     in (low + length (filter (\Pulse {..} -> pulseSignal == Low) newPulses), high + length (filter (\Pulse {..} -> pulseSignal == High) newPulses))

go (target, value) n [] machine state = go (target, value) (n + 1) [Pulse Low "button" "broadcaster"] machine state
go (target, value) n pulses machine state = {- (case DM.lookup "jq" state of
    Just (ConjunctionState values) | not (all (== Low) $ DM.elems values) -> traceShow (n, values)
    _ -> id) $ -}
    let
        results = map (\pulse -> run pulse machine state) pulses
        newState = foldr (\(name, moduleState) -> DM.insert name moduleState) state $ catMaybes $ map snd results
        newPulses = concatMap fst results
        rxPulse = filter (\Pulse {..} -> pulseSignal == value && pulseTo == target) newPulses
    in case rxPulse of
        [] -> go (target, value) n newPulses machine newState
        (_:_) -> n : (go (target, value) n newPulses machine newState)

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    startTime <- getCurrentTime

    print startTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            print values

            let
                f (name, (Nothing, _)) = Nothing
                f (name, (Just FlipFlop, _)) = Just (name, FlipFlopState Off)
                f (name, (Just Conjunction, _)) = Just (name, ConjunctionState $ DM.fromList $ map (\(name, (_, outputs)) -> (name, Low)) $ filter (\(_, (_, outputs)) -> name `elem` outputs) $ DM.toList values)
                initialState = DM.fromList $ catMaybes $ map f $ DM.toList values
                -- (low, high) = go 1000 [Pulse Low "button" "broadcaster"] values initialState
                [gt1, gt2] = take 2 (go ("gt", Low) 0 [Pulse Low "button" "broadcaster"] values initialState)
                [vr1, vr2] = take 2 (go ("vr", Low) 0 [Pulse Low "button" "broadcaster"] values initialState)
                [nl1, nl2] = take 2 (go ("nl", Low) 0 [Pulse Low "button" "broadcaster"] values initialState)
                [lr1, lr2] = take 2 (go ("lr", Low) 0 [Pulse Low "button" "broadcaster"] values initialState)

            -- print initialState
            -- print ((low + 1000), high)
            -- print ((low + 1000) * high)

            print (product [gt2 - gt1, vr2 - vr1, nl2 - nl1, lr2 - lr1])

    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))