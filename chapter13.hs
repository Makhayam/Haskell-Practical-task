import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------
-- HC13: File & Module Exercises
-----------------------------

-- HC13T1: List files in directory (simulated)
listFiles :: [String]
listFiles = ["report.txt", "data.csv", "notes.md", "summary.docx"]

-- HC13T2: Filter files by substring
filterFiles :: String -> [String] -> [String]
filterFiles sub = filter (isInfixOf sub)

-- HC13T3: Sort and return filtered files
sortedFilteredFiles :: String -> [String] -> [String]
sortedFilteredFiles sub files = sort $ filterFiles sub files

-- HC13T4 & HC13T5: SumNonEmpty module simulation
sumNonEmpty :: [Int] -> Int
sumNonEmpty [] = error "Empty list not allowed"
sumNonEmpty xs = sum xs

-- HC13T6: Convert filtered file names to Map
filesToMap :: [String] -> Map Int String
filesToMap files = Map.fromList $ zip [1..] files

-- HC13T7: Use custom SumNonEmpty function
sumExample :: Int
sumExample = sumNonEmpty [10,20,30]

-- HC13T8: Qualified imports for name conflicts (simulated)
fooA :: String
fooA = "ModuleA foo"

fooB :: String
fooB = "ModuleB foo"

qualifiedExample :: (String, String)
qualifiedExample = (fooA, fooB)

-- HC13T9: Renaming module namespace (Data.Map as M)
mapNamespaceExample :: Map Int String
mapNamespaceExample = Map.fromList [(1,"one"), (2,"two")]

-- HC13T10: Multi-module main function
multiModuleDemo :: IO ()
multiModuleDemo = do
    putStrLn "All files:"
    print listFiles
    putStrLn "Filtered files containing 'data':"
    print $ sortedFilteredFiles "data" listFiles
    putStrLn "Files mapped to keys:"
    print $ filesToMap listFiles
    putStrLn "Sum example from SumNonEmpty function:"
    print sumExample
    putStrLn "Qualified module example:"
    print qualifiedExample
    putStrLn "Renamed module (Data.Map as M) example:"
    print mapNamespaceExample

-----------------------------
-- Main
-----------------------------
main :: IO ()
main = multiModuleDemo
