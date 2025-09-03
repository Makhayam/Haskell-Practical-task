{-# LANGUAGE Safe #-}

import System.IO
import Control.Exception (catch, IOException)
import Data.List (sort)

-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

-- HC12T3: Factorial Function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC12T4: First 10 Fibonacci Numbers
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

firstNFibs :: Int -> [Integer]
firstNFibs n = map fib [0..(n-1)]

-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- HC12T6: Sort a List of Integers
sortIntegers :: [Int] -> [Int]
sortIntegers = sort

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y    = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

-- HC12T10: Math operations
add :: Int -> Int -> Int
add = (+)

sub :: Int -> Int -> Int
sub = (-)

mul :: Int -> Int -> Int
mul = (*)

divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x y = Just (x `div` y)

main :: IO ()
main = do
    -- HC12T1: Print a Welcome Message
    putStrLn "Welcome to Haskell Programming!"

    -- HC12T2: Add Two Numbers
    let sumResult = addTwoNumbers 5 7
    putStrLn $ "5 + 7 = " ++ show sumResult

    -- HC12T3: Factorial Function
    putStrLn $ "Factorial of 5 = " ++ show (factorial 5)

    -- HC12T4: First 10 Fibonacci Numbers
    putStrLn $ "First 10 Fibonacci numbers: " ++ show (firstNFibs 10)

    -- HC12T5: Palindrome Checker
    let testPalindrome = "level"
    putStrLn $ "Check if \"" ++ testPalindrome ++ "\" is a palindrome: " ++
               if isPalindrome testPalindrome then "Yes" else "No"

    -- HC12T6: Sort a List of Integers
    let nums = [5, 3, 8, 1, 2]
    putStrLn $ "Original list: " ++ show nums
    putStrLn $ "Sorted list: " ++ show (sortIntegers nums)

    -- HC12T7: Calculate Circle Area
    let radius = 3.0
    putStrLn $ "Area of circle with radius " ++ show radius ++ " = " ++ show (calculateCircleArea radius)

    -- HC12T8: Merge Two Sorted Lists
    let list1 = [1,3,5,7]
        list2 = [2,4,6,8]
    putStrLn $ "Merged list: " ++ show (mergeLists list1 list2)

    -- HC12T9: Read and Print File Content (default file)
    let filename = "example.txt"
    putStrLn $ "Attempting to read file: " ++ filename
    catch (readFile filename >>= putStrLn)
          (\e -> putStrLn $ "Error reading file: " ++ show (e :: IOException))

    -- HC12T10: Mathematical Operations Demo
    putStrLn $ "add 10 5 = " ++ show (add 10 5)
    putStrLn $ "sub 10 5 = " ++ show (sub 10 5)
    putStrLn $ "mul 10 5 = " ++ show (mul 10 5)
    putStrLn $ "divide 10 0 = " ++ show (divide 10 0)
    putStrLn $ "divide 10 2 = " ++ show (divide 10 2)
