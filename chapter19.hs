{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Data.List
import Control.Applicative

-----------------------------
-- HC19: Applicative Exercises
-----------------------------

-- HC19T1: fmap is required for Applicative
-- Already available for Functor instances

-- HC19T2: Identity law demonstration
identityLaw :: Maybe Int -> Bool
identityLaw v = (pure id <*> v) == v

-- HC19T3: pure (+1) <*> Just 2
exampleT3 :: Maybe Int
exampleT3 = pure (+1) <*> Just 2

-- HC19T4: <*> in list applies functions to all combinations
exampleT4 :: [Int]
exampleT4 = [(+1), (*2)] <*> [1,2]

-- HC19T5: pure (*) <*> [2,3] <*> [4,5]
exampleT5 :: [Int]
exampleT5 = pure (*) <*> [2,3] <*> [4,5]

-- HC19T6: Applicative is more powerful than Functor, less than Monad
-- Demonstration: sequencing without binding
exampleT6 :: [Int]
exampleT6 = (+) <$> [1,2] <*> [3,4]

-- HC19T7: pure (x -> x*2) <*> Nothing
exampleT7 :: Maybe Int
exampleT7 = pure (\x -> x*2) <*> Nothing

-- HC19T8: sequencing with <*> (list example)
exampleT8 :: [Int]
exampleT8 = [(+1), (*3)] <*> [1,2]

-- HC19T9: Applicative instance allowing validation-like combination
-- Here we simulate Either for validation
exampleT9 :: Either String Int
exampleT9 = pure (+2) <*> Right 3

-- HC19T10: pure lifts value into Applicative
exampleT10 :: Maybe Int
exampleT10 = pure 42

-----------------------------
-- Main function to display results
-----------------------------
main :: IO ()
main = do
    putStrLn "HC19 Applicative Examples:"
    print $ identityLaw (Just 5)           -- True
    print exampleT3                         -- Just 3
    print exampleT4                         -- [2,3,2,4]
    print exampleT5                         -- [8,10,12,15]
    print exampleT6                         -- [4,5,5,6]
    print exampleT7                         -- Nothing
    print exampleT8                         -- [2,3,3,6]
    print exampleT9                         -- Right 5
    print exampleT10                        -- Just 42
