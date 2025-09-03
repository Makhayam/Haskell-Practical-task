{-# LANGUAGE DeriveGeneric #-}
import Data.Semigroup
import Data.Monoid (Sum(..))
import GHC.Generics (Generic)

-----------------------------------
-- HC17T1: Severity Data Type & Semigroup
-----------------------------------
data Severity = Low | Medium | High | Critical
    deriving (Show, Eq, Ord, Enum, Bounded)

instance Semigroup Severity where
    (<>) = max

-----------------------------------
-- HC17T2: MyMin and MyMax Newtypes with Semigroup
-----------------------------------
newtype MyMin a = MyMin { getMyMin :: a } deriving (Show)
newtype MyMax a = MyMax { getMyMax :: a } deriving (Show)

instance Ord a => Semigroup (MyMin a) where
    MyMin x <> MyMin y = MyMin (min x y)

instance Ord a => Semigroup (MyMax a) where
    MyMax x <> MyMax y = MyMax (max x y)

-----------------------------------
-- HC17T3: Monoid Instance for Severity
-----------------------------------
instance Monoid Severity where
    mempty = Low
    mappend = (<>)

-----------------------------------
-- HC17T4: Monoid Instance for MySum
-----------------------------------
newtype MySum a = MySum { getSumVal :: a } deriving (Show)

instance Num a => Semigroup (MySum a) where
    MySum x <> MySum y = MySum (x + y)

instance Num a => Monoid (MySum a) where
    mempty = MySum 0
    mappend = (<>)

-----------------------------------
-- HC17T5: combineLists Function
-----------------------------------
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

-----------------------------------
-- HC17T6: maxSeverity Function
-----------------------------------
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

-----------------------------------
-- HC17T7: multiplyProducts Function
-----------------------------------
newtype ProductVal a = ProductVal { getProduct :: a } deriving (Show)

instance Num a => Semigroup (ProductVal a) where
    ProductVal x <> ProductVal y = ProductVal (x * y)

instance Num a => Monoid (ProductVal a) where
    mempty = ProductVal 1
    mappend = (<>)

multiplyProducts :: Num a => [ProductVal a] -> ProductVal a
multiplyProducts = mconcat

-----------------------------------
-- HC17T8: foldWithSemigroup using Sum wrapper
-----------------------------------
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)

-----------------------------------
-- HC17T9: Config Data Type & Semigroup
-----------------------------------
data Config = Config
    { loggingLevel :: Severity
    , timeout      :: Int
    , retries      :: Int
    } deriving (Show)

instance Semigroup Config where
    Config l1 t1 r1 <> Config l2 t2 r2 = Config
        (max l1 l2)
        (min t1 t2)
        (max r1 r2)

-----------------------------------
-- HC17T10: Monoid Instance for Config
-----------------------------------
instance Monoid Config where
    mempty = Config Low maxBound 0
    mappend = (<>)

-----------------------------------
-- Main: Demonstrate All Tasks
-----------------------------------
main :: IO ()
main = do
    putStrLn "HC17T1: Semigroup Severity"
    print $ High <> Medium <> Critical

    putStrLn "\nHC17T2: MyMin/MyMax newtypes"
    print $ MyMin 10 <> MyMin 3
    print $ MyMax 10 <> MyMax 3

    putStrLn "\nHC17T3: Monoid Severity"
    print $ mconcat [Low, Medium, High, Critical]

    putStrLn "\nHC17T4: Monoid MySum"
    print $ mconcat [MySum 1, MySum 2, MySum 3]

    putStrLn "\nHC17T5: combineLists"
    print $ combineLists [1,2,3] [4,5]

    putStrLn "\nHC17T6: maxSeverity"
    print $ maxSeverity [Low, Medium, High, Critical]

    putStrLn "\nHC17T7: multiplyProducts"
    print $ multiplyProducts [ProductVal 2, ProductVal 3, ProductVal 4]

    putStrLn "\nHC17T8: foldWithSemigroup (using Sum wrapper)"
    print $ foldWithSemigroup [Sum 1, Sum 2, Sum 3, Sum 4]

    putStrLn "\nHC17T9: Semigroup Config"
    let c1 = Config Low 30 1
        c2 = Config High 20 3
    print $ c1 <> c2

    putStrLn "\nHC17T10: Monoid Config"
    print $ mconcat [c1, c2, mempty]
