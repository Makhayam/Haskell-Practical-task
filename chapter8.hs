-- HC8T1: Type synonyms and generateTx function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to amount = "From: " ++ from ++ ", To: " ++ to ++ ", Amount: " ++ show amount

-- HC8T2: PaymentMethod and Person
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person
  { pname :: String
  , paddress :: (String, Int)
  , payment :: PaymentMethod
  } deriving Show

bob :: Person
bob = Person "Bob" ("Main Street", 123) Cash

-- HC8T3: Shape and area function
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- HC8T4: Employee with record syntax
data Employee = Employee
  { name :: String
  , experienceInYears :: Float
  } deriving Show

richard :: Employee
richard = Employee "Richard" 7.5

-- HC8T5: Person using record syntax
data PersonRecord = PersonRecord
  { prName :: String
  , prAge :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: PersonRecord
person1 = PersonRecord "Alice" 30 True

person2 :: PersonRecord
person2 = PersonRecord "John" 22 False

-- HC8T6: Shape with record syntax
data CircleShape = CircleShape
  { center :: (Float, Float)
  , radius :: Float
  , circleColor :: String
  } deriving Show

data RectangleShape = RectangleShape
  { width :: Float
  , height :: Float
  , rectColor :: String
  } deriving Show

circleExample :: CircleShape
circleExample = CircleShape (0, 0) 10 "Red"

rectangleExample :: RectangleShape
rectangleExample = RectangleShape 20 10 "Blue"

-- HC8T7: Animal type and describeAnimal function
data Animal = Dog String | Cat String

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

dog1 :: Animal
dog1 = Dog "Rex"

cat1 :: Animal
cat1 = Cat "Whiskers"

-- HC8T8: Type synonyms and greet function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet name age = "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."

-- HC8T9: Transaction and createTransaction
data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction f t a =
  let tx = Transaction f t a (f ++ "-" ++ t ++ "-" ++ show a)
  in transactionId tx

-- HC8T10: Book with Show
data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving Show

book1 :: Book
book1 = Book "The Haskell Book" "John Smith" 2020

-- Main to demonstrate everything
main :: IO ()
main = do
  putStrLn "HC8T1: generateTx"
  putStrLn (generateTx "addr1" "addr2" 100)

  putStrLn "\nHC8T2: Person bob"
  print bob

  putStrLn "\nHC8T3: Area calculations"
  print (area (Circle 5))          -- 78.5398
  print (area (Rectangle 10 5))    -- 50.0

  putStrLn "\nHC8T4: Employee richard"
  print richard

  putStrLn "\nHC8T5: person1 and person2"
  print person1
  print person2

  putStrLn "\nHC8T6: Shapes with record syntax"
  print circleExample
  print rectangleExample

  putStrLn "\nHC8T7: describeAnimal"
  putStrLn (describeAnimal dog1)
  putStrLn (describeAnimal cat1)

  putStrLn "\nHC8T8: greet"
  putStrLn (greet "Makhaya" 20)

  putStrLn "\nHC8T9: createTransaction"
  putStrLn (createTransaction "addrA" "addrB" 150)

  putStrLn "\nHC8T10: Book"
  print book1
