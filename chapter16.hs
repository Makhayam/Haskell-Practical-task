{-# LANGUAGE OverloadedStrings #-}
import Data.Char (toUpper)
import Data.List (group, sort)
import qualified Data.Map as Map

-----------------------------------
-- HC16T1: Reverse a String
-----------------------------------
reverseString :: String -> String
reverseString = reverse

-----------------------------------
-- HC16T2: Palindrome Checker
-----------------------------------
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-----------------------------------
-- HC16T3: Factorial
-----------------------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-----------------------------------
-- HC16T4: Filter Even Numbers
-----------------------------------
filterEven :: [Int] -> [Int]
filterEven = filter even

-----------------------------------
-- HC16T5: Uppercase String
-----------------------------------
toUppercase :: String -> String
toUppercase = map toUpper

-----------------------------------
-- HC16T6: nth Fibonacci Number
-----------------------------------
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-----------------------------------
-- HC16T7: Element Existence in List
-----------------------------------
existsInList :: Eq a => a -> [a] -> Bool
existsInList = elem

-----------------------------------
-- HC16T8: Insertion Sort
-----------------------------------
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y:z:zs
      | otherwise = z : insert y zs

-----------------------------------
-- HC16T9: Remove Duplicates from List
-----------------------------------
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise   = x : removeDuplicates xs

-----------------------------------
-- HC16T10: Character Frequency in String
-----------------------------------
charFrequency :: String -> Map.Map Char Int
charFrequency s = Map.fromListWith (+) [(c,1) | c <- s]

-----------------------------------
-- Main: Run all tasks with examples
-----------------------------------
main :: IO ()
main = do
    putStrLn "HC16T1: Reverse String"
    putStrLn $ reverseString "Hello"
    
    putStrLn "\nHC16T2: Palindrome Checker"
    print $ isPalindrome "racecar"
    
    putStrLn "\nHC16T3: Factorial"
    print $ factorial 5
    
    putStrLn "\nHC16T4: Filter Even Numbers"
    print $ filterEven [1,2,3,4,5,6]
    
    putStrLn "\nHC16T5: Uppercase String"
    putStrLn $ toUppercase "hello world"
    
    putStrLn "\nHC16T6: Fibonacci Number"
    print $ fibonacci 10
    
    putStrLn "\nHC16T7: Element Existence in List"
    print $ existsInList 3 [1,2,3,4]
    
    putStrLn "\nHC16T8: Insertion Sort"
    print $ insertionSort [5,3,8,1,2]
    
    putStrLn "\nHC16T9: Remove Duplicates"
    print $ removeDuplicates [1,2,3,2,4,1,5]
    
    putStrLn "\nHC16T10: Character Frequency"
    print $ charFrequency "hello world"
