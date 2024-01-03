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

import qualified Data.PSQueue as PSQ
import Data.PSQueue(Binding(..))

data Direction = L | R | U | D
    deriving (Eq, Show, Ord)

parseDirection = choice [L <$ char 'L', R <$ char 'R', D <$ char 'D', U <$ char 'U']

parseLine :: Parser (Direction, Integer)
parseLine = do
    direction <- parseDirection
    char ' '
    steps <- decimal
    string " (#"
    color <- hexadecimal
    char ')'
    -- return (direction, steps, color)
    return (case color `mod` 16 of
        0 -> R
        1 -> D
        2 -> L
        3 -> U, color `div` 16)


fileParser = sepBy1 parseLine (char '\n')

moveDirection (x,y) L steps = (x - steps, y)
moveDirection (x,y) R steps = (x + steps, y)
moveDirection (x,y) U steps = (x, y - steps)
moveDirection (x,y) D steps = (x, y + steps)

makeInterior D (x,y) = (x - 1, y)
makeInterior U (x,y) = (x + 1, y)
makeInterior L (x,y) = (x, y - 1)
makeInterior R (x,y) = (x, y + 1)

-- run :: (Integer, Integer) -> [(Direction, Integer)] -> (DS.Set (Integer, Integer), DS.Set (Integer, Integer))
run xPoints yPoints (x,y) [] = (DS.empty, DS.empty)
run xPoints yPoints (x,y) ((direction,steps):instructions) = let
        newPosition = moveDirection (x,y) direction steps
        (edge, interior) = run xPoints yPoints newPosition instructions

        gridPosition = (DM.findWithDefault (error "Impossible") x xPoints, DM.findWithDefault (error "Impossible") y yPoints)

        gridSteps = case direction of
            R -> DM.findWithDefault (error "Impossible") (fst newPosition) xPoints - DM.findWithDefault (error "Impossible") x xPoints
            L -> DM.findWithDefault (error "Impossible") x xPoints - DM.findWithDefault (error "Impossible") (fst newPosition) xPoints
            D -> DM.findWithDefault (error "Impossible") (snd newPosition) yPoints - DM.findWithDefault (error "Impossible") y yPoints
            U -> DM.findWithDefault (error "Impossible") y yPoints - DM.findWithDefault (error "Impossible") (snd newPosition) yPoints

        newEdge = DS.fromList (map (\step -> moveDirection gridPosition direction step) [0..gridSteps])
        newInterior = DS.fromList (map (\step -> moveDirection (makeInterior direction gridPosition) direction step) [0..gridSteps])
    in (DS.union edge newEdge, DS.union interior newInterior)

growInterior interior edges = let
    newInterior = concatMap (\(x,y) -> [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx == 0 || dy == 0, dx /= 0 || dy /= 0, (x + dx, y + dy) `DS.notMember` interior, (x + dx, y + dy) `DS.notMember` edges]) $ DS.toList interior
    in case newInterior of
        [] -> interior
        _ -> DS.union interior (growInterior (DS.fromList newInterior) (DS.union interior edges))

gatherPoints :: (Integer, Integer) -> [(Direction, Integer)] -> (DS.Set Integer, DS.Set Integer)
gatherPoints _ [] = (DS.empty, DS.empty)
gatherPoints (x, y) ((direction, steps):xs) = let (xPoints, yPoints) = gatherPoints (moveDirection (x,y) direction steps) xs in (DS.union (DS.fromList [x, x+1]) xPoints, DS.union (DS.fromList [y, y+1]) yPoints)

squareSize xPointsList yPointsList (x,y) = let
    width = xPointsList !! (x + 1) - xPointsList !! x
    height = yPointsList !! (y + 1) - yPointsList !! y
    in width * height

main :: IO ()
main = do
    input <- DT.readFile (replaceExtension $__FILE__ ".input")

    startTime <- getCurrentTime

    print startTime

    case parseOnly fileParser input of
        Left err -> error err
        Right values -> do
            -- mapM_ print values

            let
                minx = DS.findMin (DS.map fst grid)
                miny = DS.findMin (DS.map snd grid)
                maxx = DS.findMax (DS.map fst grid)
                maxy = DS.findMax (DS.map snd grid)

                interior = growInterior (interior_ `DS.difference` grid) grid

                points = gatherPoints (0, 0) values

                xPointsList = DS.toAscList (fst points)
                yPointsList = DS.toAscList (snd points)

                xPoints = DM.fromList (zip xPointsList [0..])
                yPoints = DM.fromList (zip yPointsList [0..])

                (grid, interior_) = run xPoints yPoints (0,0) values

            -- print (xPoints, yPoints)
            print (DS.size interior_)
            print (DS.size interior)
            print (DS.size interior + DS.size grid)

            -- forM_ [miny..maxy] $ \y -> do
            --     forM_ [minx..maxx] $ \x ->
            --         if DS.member (x,y) grid
            --             then putStr "#"
            --             else if DS.member (x,y) interior
            --                 then putStr "."
            --                 else putStr " "

            --     putStrLn ""

            -- print (map (squareSize xPointsList yPointsList) $ DS.toList (DS.union interior grid))
            print (sum (map (squareSize xPointsList yPointsList) $ DS.toList (DS.union interior grid)))

            -- print (DS.size interior_)
            -- print (DS.size interior)
            -- print (DS.size interior + DS.size grid)


    stop <- getCurrentTime
    putStrLn ("Took: " ++ show (diffUTCTime stop startTime))