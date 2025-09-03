{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Text.Read (readMaybe)

-----------------------------------
-- HC15T1: Velocity Calculation (No File)
-----------------------------------
task1 :: IO ()
task1 = do
    let distance = 100
        time = 20
    putStrLn "HC15T1:"
    if time == 0
       then putStrLn "Error: Division by zero!"
       else putStrLn $ "Velocity = " ++ show (distance / time)

-----------------------------------
-- HC15T2: Self-Driving AI Car System
-----------------------------------
driveCar :: String -> String
driveCar "Green"  = "Go forward"
driveCar "Yellow" = "Slow down"
driveCar "Red"    = "Stop"
driveCar _        = "Unknown signal"

task2 :: IO ()
task2 = do
    putStrLn "\nHC15T2:"
    mapM_ (putStrLn . driveCar) ["Green", "Yellow", "Red", "Blue"]

-----------------------------------
-- HC15T3: Custom Exception for Traffic Light Errors
-----------------------------------
data TrafficLightException = InvalidLight String
    deriving Show

instance Exception TrafficLightException

checkLight :: String -> IO ()
checkLight "Green"  = putStrLn "Go forward"
checkLight "Yellow" = putStrLn "Slow down"
checkLight "Red"    = putStrLn "Stop"
checkLight other    = throwIO (InvalidLight other)

task3 :: IO ()
task3 = do
    putStrLn "\nHC15T3:"
    (mapM_ checkLight ["Green", "Red", "Blue"]) `catch` \(InvalidLight msg) ->
        putStrLn $ "Caught exception: Invalid light - " ++ msg

-----------------------------------
-- HC15T4: Exception Handler for Traffic Light
-----------------------------------
handleLight :: String -> IO ()
handleLight "Green"  = putStrLn "Go forward"
handleLight "Yellow" = putStrLn "Slow down"
handleLight "Red"    = putStrLn "Stop"
handleLight other    = throwIO (InvalidLight other)

task4 :: IO ()
task4 = do
    putStrLn "\nHC15T4:"
    let testLights = ["Green", "Yellow", "Blue"]
    mapM_ (\c -> handleLight c `catch` \(InvalidLight msg) ->
        putStrLn $ "Error: Invalid light - " ++ msg) testLights

-----------------------------------
-- HC15T5: Safe Division Using Maybe
-----------------------------------
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

task5 :: IO ()
task5 = do
    putStrLn "\nHC15T5:"
    mapM_ print [safeDiv 10 2, safeDiv 5 0]

-----------------------------------
-- HC15T6: Safe Input Parsing with readMaybe
-----------------------------------
task6 :: IO ()
task6 = do
    putStrLn "\nHC15T6:"
    let testInputs = ["10", "abc", "25"]
    mapM_ (\i -> case readMaybe i :: Maybe Int of
                    Just n  -> print n
                    Nothing -> putStrLn "Invalid number") testInputs

-----------------------------------
-- HC15T7: Velocity Calculation with Optionals
-----------------------------------
task7 :: IO ()
task7 = do
    putStrLn "\nHC15T7:"
    let testCases = [("100","20"), ("50","0"), ("abc","10")]
    mapM_ (\(dStr,tStr) -> case (readMaybe dStr, readMaybe tStr) of
        (Just d, Just t) -> if t == 0
                            then putStrLn "Error: Division by zero"
                            else putStrLn $ "Velocity = " ++ show (d / t)
        _ -> putStrLn "Error: Invalid input") testCases

-----------------------------------
-- HC15T8: Division with Either
-----------------------------------
safeDivEither :: Double -> Double -> Either String Double
safeDivEither _ 0 = Left "Division by zero"
safeDivEither x y = Right (x / y)

task8 :: IO ()
task8 = do
    putStrLn "\nHC15T8:"
    mapM_ print [safeDivEither 10 2, safeDivEither 5 0]

-----------------------------------
-- HC15T9: Try Function (No File, Simulated)
-----------------------------------
task9 :: IO ()
task9 = do
    putStrLn "\nHC15T9:"
    let content = "Simulated file content: 100 20"
    putStrLn content

-----------------------------------
-- HC15T10: Hybrid Error Handling with Either
-----------------------------------
task10 :: IO ()
task10 = do
    putStrLn "\nHC15T10:"
    let distance = 100
        time = 20
    case safeDivEither distance time of
        Left msg -> putStrLn msg
        Right v  -> putStrLn $ "Velocity = " ++ show v

-----------------------------------
-- Main: Run all tasks
-----------------------------------
main :: IO ()
main = do
    task1
    task2
    task3
    task4
    task5
    task6
    task7
    task8
    task9
    task10
