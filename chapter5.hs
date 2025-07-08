-- HC5T1: Apply a function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- HC5T2: Filter odd numbers from 1 to 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

-- HC5T3: Check for any word that starts with an uppercase letter
hasUppercaseStart :: [String] -> Bool
hasUppercaseStart = any (\word -> not (null word) && head word `elem` ['A'..'Z'])

-- HC5T4: Rewrite using lambda
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5: Partial application to multiply by 5
multiplyByFive :: Int -> Int
multiplyByFive = (*) 5

-- HC5T6: Compose functions to square then filter even results
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- HC5T7: Rewrite using $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8: Convert to point-free style
addFive :: Int -> Int
addFive = (+ 5)

-- HC5T9: Apply a function twice to every list element
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10: Check if any square in list is greater than 50
anySquareAbove50 :: [Int] -> Bool
anySquareAbove50 = any (>50) . map (^2)

-- Test everything in main
main :: IO ()
main = do
  -- HC5T1
  print $ applyThrice (+1) 5         -- should be 8

  -- HC5T2
  print oddNumbers                   -- [1,3,5,...,29]

  -- HC5T3
  print $ hasUppercaseStart ["hello", "World", "test"] -- True
  print $ hasUppercaseStart ["hello", "world"]         -- False

  -- HC5T4
  print $ biggerThan10 15            -- True
  print $ biggerThan10 8             -- False

  -- HC5T5
  print $ multiplyByFive 6           -- 30

  -- HC5T6
  print $ evenSquares [1..10]        -- [4,16,36,64,100]

  -- HC5T7
  print result                       -- 92

  -- HC5T8
  print $ addFive 10                 -- 15

  -- HC5T9
  print $ transformList (+2) [1,2,3] -- [5,6,7]

  -- HC5T10
  print $ anySquareAbove50 [3,5,8]   -- True (64)
  print $ anySquareAbove50 [1,2,3]   -- False
