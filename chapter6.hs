-- HC6T1: Recursive factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC6T2: Recursive Fibonacci
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- HC6T3: Sum using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

-- HC6T4: Product using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

-- HC6T5: Reverse a list using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- HC6T6: Check if an element exists in a list
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists y (x:xs)
  | y == x    = True
  | otherwise = elementExists y xs

-- HC6T7: Length of a list
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- HC6T8: Filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens [] = []
filterEvens (x:xs)
  | even x    = x : filterEvens xs
  | otherwise = filterEvens xs

-- HC6T9: Map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- HC6T10: Recursive function to get digits of a number
digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- Optional main for testing
main :: IO ()
main = do
  print $ factorial 5           -- 120
  print $ fibonacci 10          -- 55
  print $ sumList [1,2,3,4]     -- 10
  print $ productList [1,2,3,4] -- 24
  print $ reverseList [1,2,3]   -- [3,2,1]
  print $ elementExists 2 [1,3,5] -- False
  print $ elementExists 3 [1,3,5] -- True
  print $ listLength [1,2,3,4]  -- 4
  print $ filterEvens [1..10]   -- [2,4,6,8,10]
  print $ myMap (+1) [1,2,3]    -- [2,3,4]
  print $ digits 12345          -- [1,2,3,4,5]
