module Main where

import Data.Char (toLower)

---------------------------
-- HC18T1: mapToLower
---------------------------
mapToLower :: String -> String
mapToLower = fmap toLower

---------------------------
-- HC18T2 & HC18T3: Tree and incrementTreeValues
---------------------------
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

---------------------------
-- HC18T4: mapToBits
---------------------------
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

---------------------------
-- HC18T5: Functor for Either is standard; just use fmap
---------------------------

---------------------------
-- HC18T6: applyToMaybe
---------------------------
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

---------------------------
-- HC18T7: fmapTuple
---------------------------
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

---------------------------
-- HC18T8: identityLawCheck
---------------------------
identityLawCheck :: (Eq (f a), Functor f) => f a -> Bool
identityLawCheck x = fmap id x == id x

---------------------------
-- HC18T9: compositionLawCheck
---------------------------
compositionLawCheck :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x

---------------------------
-- HC18T10: nestedFmap
---------------------------
nestedFmap :: (a -> b) -> [[Maybe a]] -> [[Maybe b]]
nestedFmap = fmap . fmap . fmap

---------------------------
-- MAIN
---------------------------
main :: IO ()
main = do
    putStrLn "HC18T1: mapToLower"
    print $ mapToLower "HELLO Functor"

    putStrLn "\nHC18T2 & HC18T3: Tree and incrementTreeValues"
    let tree = Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)
    print tree
    print $ fmap (*2) tree
    print $ incrementTreeValues tree

    putStrLn "\nHC18T4: mapToBits"
    print $ mapToBits [True, False, True, True]

    putStrLn "\nHC18T5: Functor Either"
    print $ fmap (+1) (Right 10 :: Either String Int)
    print $ fmap (+1) (Left "Error" :: Either String Int)

    putStrLn "\nHC18T6: applyToMaybe"
    print $ applyToMaybe (+1) (Just 4)
    print $ applyToMaybe (+1) Nothing

    putStrLn "\nHC18T7: fmapTuple"
    print $ fmapTuple length ("Hello", "World")

    putStrLn "\nHC18T8: identityLawCheck"
    print $ identityLawCheck (Just 42)
    print $ identityLawCheck [1,2,3]

    putStrLn "\nHC18T9: compositionLawCheck"
    print $ compositionLawCheck (*2) (+3) (Just 5)
    print $ compositionLawCheck (*2) (+3) [1,2,3]

    putStrLn "\nHC18T10: nestedFmap"
    print $ nestedFmap (+1) [[Just 1, Nothing], [Just 2, Just 3]]
