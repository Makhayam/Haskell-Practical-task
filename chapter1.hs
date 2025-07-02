-- Import required for sortBy
import Data.List (sortBy)

-- HC1T1 - Task 1: Function Composition
double :: Num a => a -> a
double x = x * 2

increment :: Num a => a -> a
increment x = x + 1

doubleThenIncrement :: Num a => a -> a
doubleThenIncrement = increment . double

-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: (Ord a, Num a) => a -> Bool
greaterThan18 n = n > 18

-- HC1T4 - Task 4: Composing a Function to Process Player Data
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers players = [name | (name, _) <- players]

sortByScore :: [Player] -> [Player]
sortByScore = reverse . sortBy (\(_, s1) (_, s2) -> compare s1 s2)

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Integer]
infiniteNumbers = [1..]

firstN :: Int -> [Integer]
firstN n = take n infiniteNumbers

-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * 5 / 9

-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Main Function to Demonstrate Output
main :: IO ()
main = do
  -- HC1T1
  putStrLn "HC1T1: doubleThenIncrement 5"
  print $ doubleThenIncrement 5     -- 11

  -- HC1T2
  putStrLn "\nHC1T2: circleArea 3"
  print $ circleArea 3              -- ~28.27

  -- HC1T3
  putStrLn "\nHC1T3: greaterThan18 20 and 10"
  print $ greaterThan18 20          -- True
  print $ greaterThan18 10          -- False

  -- HC1T4
  putStrLn "\nHC1T4: getTopThreePlayers"
  let players = [("Alice", 40), ("Bob", 90), ("Charlie", 60), ("Diana", 80)]
  print $ getTopThreePlayers players  -- ["Bob", "Diana", "Charlie"]

  -- HC1T5
  putStrLn "\nHC1T5: firstN 5"
  print $ firstN 5                  -- [1,2,3,4,5]

  -- HC1T6
  putStrLn "\nHC1T6: addNumbers 3 7"
  print $ addNumbers 3 7           -- 10

  -- HC1T7
  putStrLn "\nHC1T7: fToC 98.6"
  print $ fToC 98.6                -- ~37.0

  -- HC1T8
  putStrLn "\nHC1T8: applyTwice (+2) 5"
  print $ applyTwice (+2) 5        -- 9
