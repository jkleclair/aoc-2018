module Day1 where

import Text.Read (readMaybe)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Paths_aoc2018

strToInt [] = Nothing
strToInt (x:xs)
    | x == '+'  = int
    | x == '-'  = fmap negate int
    | otherwise = Nothing
    where
        int = readMaybe xs

findDup :: Maybe [Int] -> Set.Set Int -> Int -> Maybe Int
findDup (Just (freq:freqs)) set sum 
    | sum `Set.member` set = Just sum
    | otherwise            = findDup (Just freqs) (Set.insert sum set) newSum
    where
        newSum = sum + freq

extendList (Just xs) = Just (cycle xs)

execute = do
    filePath <- getDataFileName "inputs/day1-input.txt"
    freqsFile <- readFile filePath
    let freqsStrArr = lines freqsFile
    let freqsIntArr = map strToInt freqsStrArr  -- [Just x, Just y, ..., Just z]
    print ("Part 1:")
    print (foldl (+) 0 <$> sequence freqsIntArr)
    print ("Part 2:")
    print (findDup (extendList (sequence freqsIntArr)) Set.empty 0)