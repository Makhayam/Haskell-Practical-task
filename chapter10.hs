-- HC10T1: ShowSimple Type Class
class ShowSimple a where
    showSimple :: a -> String

data PaymentMethod = Cash | CreditCard | PayPal deriving Show

instance ShowSimple PaymentMethod where
    showSimple Cash       = "Cash"
    showSimple CreditCard = "Credit Card"
    showSimple PayPal     = "PayPal"

-- HC10T2: Summable Type Class
class Summable a where
    sumUp :: [a] -> a

instance Summable Int where
    sumUp = sum

-- HC10T3: Comparable Type Class
class Comparable a where
    compareWith :: a -> a -> Ordering

data Blockchain = Block Int | Chain Blockchain Blockchain deriving Show

instance Comparable Blockchain where
    compareWith (Block a) (Block b)         = compare a b
    compareWith (Chain a1 b1) (Chain a2 b2) = compareWith a1 a2 <> compareWith b1 b2
    compareWith (Block _) (Chain _ _)       = LT
    compareWith (Chain _ _) (Block _)       = GT

-- HC10T4: Eq Instance for Box
data Box a = Box a

instance Eq a => Eq (Box a) where
    (Box x) == (Box y) = x == y

-- HC10T5: ShowDetailed Type Class
class ShowDetailed a where
    showDetailed :: a -> String

data User = User { userId :: Int, userName :: String }

instance ShowDetailed User where
    showDetailed (User id name) = "User ID: " ++ show id ++ ", Name: " ++ name

-- HC10T6: Mutual Recursion in Eq for Blockchain
instance Eq Blockchain where
    a == b = not (a /= b)
    a /= b = not (a == b)

-- HC10T7: Convertible Type Class
class Convertible a b where
    convert :: a -> b

instance Convertible PaymentMethod String where
    convert = showSimple

-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool
    compareEquality x y = x == y -- default implementation

instance AdvancedEq Int

-- HC10T9: MinMax Type Class
class MinMax a where
    minValue :: a
    maxValue :: a

instance MinMax Int where
    minValue = minBound
    maxValue = maxBound

-- HC10T10: Concatenatable Type Class
class Concatenatable a where
    concatWith :: a -> a -> a

instance Concatenatable String where
    concatWith = (++)

-- Main function to demonstrate usage
main :: IO ()
main = do
    putStrLn $ "ShowSimple (CreditCard): " ++ showSimple CreditCard
    putStrLn $ "Summable Int: " ++ show (sumUp [1, 2, 3, 4 :: Int])
    
    let bc1 = Block 10
    let bc2 = Block 20
    print $ "Comparable Blockchain: " ++ show (compareWith bc1 bc2)
    
    print $ "Box equality: " ++ show (Box 5 == Box 5)
    
    let user = User 1 "Makhaya"
    putStrLn $ "ShowDetailed User: " ++ showDetailed user
    
    putStrLn $ "Convertible PaymentMethod to String: " ++ convert PayPal
    
    putStrLn $ "AdvancedEq Int: " ++ show (compareEquality (5 :: Int) 5)
    
    putStrLn $ "MinMax Int: min=" ++ show (minValue :: Int) ++ ", max=" ++ show (maxValue :: Int)
    
    putStrLn $ "Concatenatable String: " ++ concatWith "Hello, " "world!"
