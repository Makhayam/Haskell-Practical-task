-- HC4T1 - Task 1: Define a weatherReport Function
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2 - Task 2: Define a dayType Function
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"

-- HC4T3 - Task 3: Define a gradeComment Function
gradeComment :: Int -> String
gradeComment n
  | n >= 90 && n <= 100 = "Excellent!"
  | n >= 70 && n <= 89  = "Good job!"
  | n >= 50 && n <= 69  = "You passed."
  | n >= 0  && n <= 49  = "Better luck next time."
  | otherwise           = "Invalid grade"

-- HC4T4 and HC4T5 - Task 4 & 5: Rewrite specialBirthday using Pattern Matching & Catch-All
specialBirthday :: Int -> String
specialBirthday 1  = "Happy 1st birthday!"
specialBirthday 16 = "Sweet 16! Enjoy your day!"
specialBirthday 18 = "You're officially an adult!"
specialBirthday 21 = "Cheers to 21 years!"
specialBirthday age = "Happy birthday! You're " ++ show age ++ " years old."

-- HC4T6 - Task 6: Identify List Contents Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []        = "The list is empty."
whatsInsideThisList [_]       = "The list has one element."
whatsInsideThisList [_, _]    = "The list has two elements."
whatsInsideThisList (_:_:_:_) = "The list has three or more elements."

-- HC4T7 - Task 7: Ignore Elements in a List, return first and third elements
firstAndThird :: [a] -> Maybe (a, a)
firstAndThird (x:_:z:_) = Just (x, z)
firstAndThird _         = Nothing

-- HC4T8 - Task 8: Extract Values from Tuples and describe
describeTuple :: (Show a, Show b) => (a, b) -> String
describeTuple (x, y) = "The tuple contains: " ++ show x ++ " and " ++ show y

-- Test in main function
main :: IO ()
main = do
  -- HC4T1 tests
  print $ weatherReport "sunny"
  print $ weatherReport "rainy"
  print $ weatherReport "cloudy"
  print $ weatherReport "stormy"

  -- HC4T2 tests
  print $ dayType "Saturday"
  print $ dayType "Monday"
  print $ dayType "Funday"

  -- HC4T3 tests
  print $ gradeComment 95
  print $ gradeComment 75
  print $ gradeComment 55
  print $ gradeComment 40
  print $ gradeComment 120

  -- HC4T4 & HC4T5 tests
  print $ specialBirthday 1
  print $ specialBirthday 18
  print $ specialBirthday 30

  -- HC4T6 tests
  print $ whatsInsideThisList ([] :: [Int])
  print $ whatsInsideThisList [1]
  print $ whatsInsideThisList [1,2]
  print $ whatsInsideThisList [1,2,3,4]

  -- HC4T7 tests
  print $ firstAndThird [10, 20, 30, 40]
  print $ firstAndThird [10, 20]

  -- HC4T8 tests
  print $ describeTuple (5, "hello")
  print $ describeTuple ('a', True)
