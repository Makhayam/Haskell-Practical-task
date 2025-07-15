-- HC9T1: Parametric Type Synonym
type Entity a = (String, a)  -- (Name, Address or other detail)

-- HC9T2: Parametric Data Type Box
data Box a = Empty | Has a deriving (Show)

-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (x + n)
addN _ Empty   = Empty

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract _ (Has x) = x
extract def Empty = def

-- HC9T5: Parametric Data Type Shape with Record Syntax
data Shape a = 
    Circle { color :: a, radius :: Double } |
    Rectangle { color :: a, width :: Double, height :: Double } 
    deriving (Show)

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet {
    content  :: String,
    likes    :: Int,
    comments :: [Tweet]
} deriving (Show)

-- HC9T7: Engagement Function
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- HC9T8: Recursive Sequence Data Type
data Sequence a = End | Node a (Sequence a) deriving (Show)

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y ys) = x == y || elemSeq x ys

-- HC9T10: Binary Search Tree
data BST a = Leaf | Branch a (BST a) (BST a) deriving (Show)

-- Main function to test examples
main :: IO ()
main = do
    -- HC9T1
    let person :: Entity String
        person = ("Alice", "123 Main Street")
    print person

    -- HC9T2, HC9T3, HC9T4
    let box = Has 10
    print (addN 5 box)
    print (extract 0 box)
    print (extract 0 Empty)

    -- HC9T5
    let c = Circle { color = "Red", radius = 5.0 }
    let r = Rectangle { color = "Blue", width = 4.0, height = 6.0 }
    print c
    print r

    -- HC9T6, HC9T7
    let comment1 = Tweet "Nice post!" 3 []
    let comment2 = Tweet "Interesting!" 2 []
    let mainTweet = Tweet "Hello World" 10 [comment1, comment2]
    print (engagement mainTweet)

    -- HC9T8, HC9T9
    let seq1 = Node 1 (Node 2 (Node 3 End))
    print (elemSeq 2 seq1)
    print (elemSeq 5 seq1)

    -- HC9T10
    let tree = Branch 10 (Branch 5 Leaf Leaf) (Branch 15 Leaf Leaf)
    print tree
