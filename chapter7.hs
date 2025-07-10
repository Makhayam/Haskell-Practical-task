-- HC7T1: Define Color and implement Eq
data Color = Red | Green | Blue deriving (Show, Read, Enum, Bounded)

instance Eq Color where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    _ == _ = False

-- HC7T2: Implement Ord for Color (Red < Green < Blue)
instance Ord Color where
    compare Red Green = LT
    compare Red Blue = LT
    compare Green Blue = LT
    compare a b
        | a == b    = EQ
        | otherwise = GT

-- HC7T3: compareValues with Eq and Ord
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

-- HC7T4: Shape with Show and Read
data Shape = Circle Double | Rectangle Double Double

instance Show Shape where
    show (Circle r) = "Circle " ++ show r
    show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
    readsPrec _ input =
        case words input of
            ["Circle", r] ->
                [(Circle (read r), "")]
            ["Rectangle", w, h] ->
                [(Rectangle (read w) (read h), "")]
            _ -> []

-- Helper: Calculate area of a shape
shapeArea :: Shape -> Double
shapeArea (Circle r) = pi * r * r
shapeArea (Rectangle w h) = w * h

-- Eq and Ord instances for Shape based on area
instance Eq Shape where
    s1 == s2 = shapeArea s1 == shapeArea s2

instance Ord Shape where
    compare s1 s2 = compare (shapeArea s1) (shapeArea s2)

-- HC7T5: squareArea using Num
squareArea :: Num a => a -> a
squareArea side = side * side

-- HC7T6: circleCircumference using Integral and Floating
circleCircumference :: (Real a, Floating b) => a -> b
circleCircumference r = 2 * pi * (realToFrac r)

-- HC7T7: nextColor using Bounded and Enum
nextColor :: Color -> Color
nextColor color =
    if color == maxBound then minBound else succ color

-- HC7T8: parseShape using Read
parseShape :: String -> Maybe Shape
parseShape s =
    case reads s of
        [(shape, "")] -> Just shape
        _             -> Nothing

-- HC7T9: Describable type class with instances for Bool and Shape
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True = "This is True."
    describe False = "This is False."

instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle w h) = "A rectangle " ++ show w ++ " x " ++ show h

-- HC7T10: describeAndCompare with constraints
describeAndCompare :: (Describable a, Eq a, Ord a) => a -> a -> String
describeAndCompare x y = describe (compareValues x y)

-- Main function to demonstrate everything
main :: IO ()
main = do
    putStrLn "HC7T1 & HC7T2: Color equality and order"
    print (Red == Red)         -- True
    print (Red < Green)        -- True
    print (compare Red Blue)   -- LT

    putStrLn "\nHC7T3: compareValues"
    print (compareValues 5 10)          -- 10
    print (compareValues "apple" "bat") -- "bat"

    putStrLn "\nHC7T4: Show and Read for Shape"
    let s1 = Circle 3.5
    let s2 = Rectangle 4 5
    print s1
    print s2
    print (read "Circle 3.5" :: Shape)

    putStrLn "\nHC7T5: squareArea"
    print (squareArea 4)       -- 16

    putStrLn "\nHC7T6: circleCircumference"
    print (circleCircumference (7 :: Int))  -- ≈ 43.98

    putStrLn "\nHC7T7: nextColor"
    print (nextColor Red)      -- Green
    print (nextColor Blue)     -- Red

    putStrLn "\nHC7T8: parseShape"
    print (parseShape "Circle 5.0")     -- Just (Circle 5.0)
    print (parseShape "Triangle 5")     -- Nothing

    putStrLn "\nHC7T9: describe Bool and Shape"
    putStrLn (describe True)
    putStrLn (describe s2)

    putStrLn "\nHC7T10: describeAndCompare"
    putStrLn (describeAndCompare (Rectangle 2 5) (Rectangle 4 6))
