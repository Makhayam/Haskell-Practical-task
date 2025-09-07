{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Identity

-- HC20T1: safeDivide
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- HC20T2: sequenceMaybe
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = sequence

-- HC20T3: Writer Monad logging calculator
addLog :: Int -> Int -> Writer [String] Int
addLog x y = writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

-- HC20T4: countChars with State Monad
countChars :: Char -> String -> State Int ()
countChars c str = put $ length (filter (== c) str)

-- HC20T5: Reader Monad greeting
type Config = String
greet :: Reader Config String
greet = do
  name <- ask
  return ("Hello, " ++ name)

-- HC20T6: doubleMonad combining Maybe and List
doubleMonad :: [Maybe a] -> [a]
doubleMonad xs = [y | Just y <- xs]

-- HC20T7: findFirst with Either
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "Not found"
findFirst p (x:xs)
  | p x       = Right x
  | otherwise = findFirst p xs

-- HC20T8: Parser Monad (simple)
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (a,s') <- p s
    return (f a, s')
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a,s)
  Parser pf <*> Parser pa = Parser $ \s -> do
    (f,s') <- pf s
    (a,s'') <- pa s'
    return (f a,s'')
instance Monad Parser where
  Parser pa >>= f = Parser $ \s -> do
    (a,s') <- pa s
    runParser (f a) s'

-- HC20T9: replicateMonad with Identity
replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = Identity (replicate n x)

-- HC20T10: Nested StateT and MaybeT
nested :: StateT Int (MaybeT IO) ()
nested = do
  n <- get
  lift $ MaybeT $ return (if n > 0 then Just () else Nothing)

-- HC20T12: File reading with IO
readFileIO :: FilePath -> IO ()
readFileIO path = do
  contents <- readFile path
  putStrLn contents

-- HC20T13: fibonacciMemo with State Monad
fibonacciMemo :: Int -> State (Map Int Int) Int
fibonacciMemo 0 = return 0
fibonacciMemo 1 = return 1
fibonacciMemo n = do
  memo <- get
  case Map.lookup n memo of
    Just v -> return v
    Nothing -> do
      a <- fibonacciMemo (n-1)
      b <- fibonacciMemo (n-2)
      let v = a+b
      modify (Map.insert n v)
      return v

-- HC20T14: mapMFilter
mapMFilter :: Monad m => (a -> m Bool) -> [a] -> m [a]
mapMFilter p xs = fmap catMaybes $ mapM (\x -> do
  b <- p x
  return (if b then Just x else Nothing)) xs
  where catMaybes = foldr (\x acc -> maybe acc (:acc) x) []

-- HC20T15: treeSum with custom monad
data Tree a = Leaf a | Node (Tree a) (Tree a)
treeSum :: Tree Int -> Identity Int
treeSum (Leaf x) = Identity x
treeSum (Node l r) = Identity $ runIdentity (treeSum l) + runIdentity (treeSum r)

-- HC20T16: retryIO
retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _ = return Nothing
retryIO n action = do
  result <- action
  return (Just result)

-- HC20T17: validatePassword with Either
validatePassword :: String -> Either String String
validatePassword pwd
  | length pwd < 6 = Left "Too short"
  | not (any (`elem` ['0'..'9']) pwd) = Left "No digit"
  | otherwise = Right pwd

-- HC20T18: MaybeT for IO validation
maybeTExample :: MaybeT IO Int
maybeTExample = return 42 -- predefined value

-- HC20T19: Writer logging system
logFunction :: Int -> Writer [String] Int
logFunction x = writer (x*2, ["Doubled " ++ show x])

-- HC20T20: batchProcessing
batchProcessing :: Monad m => [m a] -> m [a]
batchProcessing = foldr (\a b -> a >>= (\x -> b >>= (\xs -> return (x:xs)))) (return [])

-- Main demonstration
main :: IO ()
main = do
  -- T1
  print $ safeDivide 10 2
  print $ safeDivide 10 0
  -- T2
  print $ sequenceMaybe [Just 1, Just 2, Just 3]
  print $ sequenceMaybe [Just 1, Nothing, Just 3]
  -- T3
  print $ runWriter (addLog 3 4)
  -- T4
  print $ execState (countChars 'a' "banana") 0
  -- T5
  putStrLn $ runReader greet "Alice"
  -- T6
  print $ doubleMonad [Just 1, Nothing, Just 3]
  -- T7
  print $ findFirst even [1,3,5,6,7]
  print $ findFirst (>10) [1,2,3]
  -- T9
  print $ runIdentity $ replicateMonad 5 "hi"
  -- T13
  print $ evalState (fibonacciMemo 10) Map.empty
  -- T15
  print $ runIdentity $ treeSum (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))
  -- T19
  print $ runWriter $ logFunction 7
  putStrLn "All other functions (like nested monads, file reading, and mapMFilter) are defined but not run for simplicity."
