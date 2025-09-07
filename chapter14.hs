{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Char
import Data.List
import Data.Maybe

-----------------------------
-- HC14T1: Hello Cabal
-----------------------------
helloCabal :: IO ()
helloCabal = putStrLn "Hello, Cabal!"

-----------------------------
-- HC14T2: Random number (simulated)
-- Since we avoid System.Random, use a fixed number
-----------------------------
printRandom :: IO ()
printRandom = putStrLn $ "Random number: " ++ show (42 :: Int)

-----------------------------
-- HC14T3: Numeric underscores
-----------------------------
largeNumbers :: IO ()
largeNumbers = do
    let bigNum = 1_000_000
        biggerNum = 12_345_678_901
    putStrLn $ "bigNum = " ++ show bigNum
    putStrLn $ "biggerNum = " ++ show biggerNum

-----------------------------
-- HC14T4: TypeApplications to read Int from String
-----------------------------
readInt :: String -> Int
readInt s = read @Int s

readIntExample :: IO ()
readIntExample = putStrLn $ "Read 123 as Int: " ++ show (readInt "123")

-----------------------------
-- HC14T5: Custom data type and pattern matching with @
-----------------------------
data Result a = Success a | Failure String deriving Show

patternMatchExample :: Result Int -> String
patternMatchExample r@(Success n) = "Success: " ++ show n ++ ", original: " ++ show r
patternMatchExample (Failure msg) = "Failure: " ++ msg

-----------------------------
-- HC14T8: Character frequency function
-----------------------------
counts :: String -> [(Char, Int)]
counts str = map (\c -> (c, length $ filter (==c) str)) (nub str)

countsExample :: IO ()
countsExample = print $ counts "hello world"

-----------------------------
-- HC14T9: PartialTypeSignatures
-----------------------------
sumPartial :: _ -> _ -> _
sumPartial x y = x + y

sumPartialExample :: IO ()
sumPartialExample = print $ sumPartial 5 7

-----------------------------
-- HC14T10: Test simulation for counts function
-----------------------------
testCounts :: Bool
testCounts = counts "aab" == [('a',2),('b',1)]

testCountsExample :: IO ()
testCountsExample = putStrLn $ "counts test passed? " ++ show testCounts

-----------------------------
-- Main
-----------------------------
main :: IO ()
main = do
    helloCabal
    printRandom
    largeNumbers
    readIntExample
    putStrLn $ patternMatchExample (Success 100)
    putStrLn $ patternMatchExample (Failure "error occurred")
    countsExample
    sumPartialExample
    testCountsExample
