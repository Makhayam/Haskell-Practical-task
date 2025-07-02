import Data.Char

-- HC2T2
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- HC2T3
myAge :: Int
myAge = 21

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- HC2T5
circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- HC2T6
smallNumber :: Int
smallNumber = 2^62

bigNumber :: Integer
bigNumber = 2^127

-- Main for testing
main :: IO ()
main = do
  print $ add 4 5
  print $ isEven 4
  print $ concatStrings "Hello, " "World!"
  print $ circleArea 3
  print $ maxOfThree 2 9 5
  print smallNumber
  print bigNumber
  print $ True && True
  print $ False || False
  print $ not False
  print $ 5 > 10
