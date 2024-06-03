-- Simona Ceskova xcesko00
-- FLP projekt 1
-- 30.03.2024

import System.Environment
import Data.List.Split
import Data.List
import Data.Ord (comparing)

main::IO()
main = do  
        args <- getArgs
        let param = parseArgs args
        if length param == 2
                then do
                        --the first part of the project
                        cfileTree <- readFile $ head param
                        cfileData <- readFile $ head (tail param)

                        let dataDouble = map (map read) ( (map (splitOn ",") (lines cfileData)))::[[Double]]
                        let splitted = map (splitOn ",") (map deleteWhiteSpace (lines cfileTree))
                        let dtree = map stringToIntDTree splitted

                        callSearching dtree dataDouble
                else do
                        --the second part of the project
                        cfileData <- readFile (head param)
                        let toPaste = (map parseToStruct $ lines cfileData)
                        generateTree toPaste 0

--arguments check
parseArgs :: [String] -> [String]
parseArgs [] = error "Wrong parameters"
parseArgs (x:xs)
        | (length x) < 2 = error "Too few parameters"
        | (length x) > 3 = error "Too many parameters"
        | ((x == "-1") && (length xs == 2)) = xs
        | ((x == "-2") && (length xs == 1)) = xs
        | otherwise = error "Wrong parameters"

-- ---------------------------------------- FIRST PART ----------------------------------------------
--tree structure
data DTree = Leaf {clas::String, rank::Int, name::String} | Node {clas::String, rank::Int, index::Int, value::Double} deriving(Show, Eq, Ord)

--function to call searchDTree for every line of input
callSearching :: [[DTree]] -> [[Double]] -> IO ()
callSearching _ [] = return ()
callSearching dtree (x:xs) = searchDTree dtree (head (head dtree)) 0 x 
        >> callSearching dtree xs

--main function for searching classes, called recursively for every next node
--the printing is here, thats why it is IO() and do
searchDTree :: [[DTree]] -> DTree -> Int -> [Double] -> IO ()
searchDTree dtree dtreeHead rankT dataT
        | clas dtreeHead == "N" =
                if value dtreeHead <= dataT !! index dtreeHead
                        then do
                                let skippedTree = skip (drop 2 dtree) (rankT + 1)
                                searchDTree skippedTree (head(head skippedTree)) (rankT + 1) dataT
                else searchDTree (tail dtree) (head(head(tail dtree))) (rankT + 1) dataT
        | otherwise = putStrLn (name dtreeHead)

--skipping other nodes and leaves because it needs to move to right side of the tree
skip :: [[DTree]] -> Int ->  [[DTree]]
skip [] _ = error "Tree should not be empty"
skip (x:xs) stopRank
        | (rank (head x)) == stopRank = (x:xs)
        | otherwise = skip xs stopRank

--parsing into structure
stringToIntDTree :: [String] -> [DTree]
stringToIntDTree x
        | "L" == head x = [(Leaf (x !! 0) (read (x !! 1)::Int) (x !! 2))]
        | "N" == head x = [(Node (x !! 0) (read (x !! 1)::Int) (read (x !! 2)::Int) (read (x !! 3)::Double))]
        | otherwise = []

--deleting unwanted characters and parsing
deleteWhiteSpace:: [Char] -> [Char]
deleteWhiteSpace x
        | ('L' `elem` x) = "L,"++(show ((length(takeWhile ('L'>) x))`div` 2)) ++ "," ++ (dropWhile (<=' ') (dropWhile (>=':') (dropWhile (<'L') x))) 
        | ('N' `elem` x) = "N,"++(show ((length(takeWhile ('N'>) x))`div` 2)) ++ "," ++ (dropWhile (>=':') (dropWhile (<'N') x))
        | otherwise = []


-- ---------------------------------------- SECOND PART ----------------------------------------------
--structure for input data
data TestData = TestData{ clasR :: String, valuesR:: [Double] } deriving(Show, Eq, Ord)

--the main function of second project part
--calls itself recursively for every new threshold and prints out the final tree
--this functions prints everything thats why it is do..., because od IO()
generateTree :: [TestData] -> Int -> IO()
generateTree toPaste counter = do 
        let minG = [callGini toPaste x | x <- [0,((length (valuesR (head toPaste)))-1)]] --calculating best separatin into two parts
        let (_,_,coll) = Prelude.minimum(minG)
        let (treshholdV,firstV,secondV) = (threshold toPaste $ Prelude.minimum(minG))
        -- more down through the tree branches
        if length(removeDuplicates(map clasR (firstV))) == 1 --in the first part is only one class so it needs to be printed
                then do
                        --separates the two parts of input, prints node and if there is only one class, then prints also leaf
                        --also here the node has to be printed as well, because before everyleaf is one node
                        putStrLn $ (printLN counter 'N') ++ (show coll) ++ ", " ++ (show ( treshholdV))
                        putStrLn $ (printLN counter 'L') ++ (clasR (head firstV))
                else do
                        putStrLn $ (printLN counter 'N') ++ (show coll) ++ ", " ++ (show ( treshholdV))
                        --no leaf to print, more separating into nodes needed
                        generateTree firstV (counter+1)
        
        if length(removeDuplicates(map clasR (secondV))) == 1 --in the second part is only one class so it needs to be printed
                --no node printing on this side
                then putStrLn $ (printLN counter 'L') ++ (clasR (head secondV))  --separates the two parts of input, prints node and if there is only one class, then prints also leaf
                else do
                        generateTree secondV (counter+1) --no leaf to print, more separating into nodes needed

-- help function for counting spaces and returning string of them
giveMeSpace :: Int -> String
giveMeSpace n
        | n > 0 = "  " ++ giveMeSpace (n-1)
        | otherwise = ""

-- help function for printing node or leaf
printLN :: Int -> Char -> String
printLN spaces x
        | x == 'N' = giveMeSpace(spaces) ++ "Node: "
        | x == 'L' = giveMeSpace(spaces) ++ "  Leaf: "
        |otherwise = ""

-- calculating threshold for node from gini index
threshold :: [TestData] -> (Double, Int, Int) -> (Double, [TestData], [TestData])
threshold x (_, idx, coll) = ((giveAverage (map valuesR (dataTuple)))!!idx, first, second)
        where
                dataTuple = sortBy (comparing ((!!coll) .valuesR)) x
                first = (take (idx+1) dataTuple)
                second = (drop (idx+1) dataTuple)

-- recursive function for getting best separation into two parts
-- returns (double, int, int) = (value, separation, collumn)
callGini :: [TestData] -> Int-> (Double, Int, Int)
callGini x n = (a!!(indexG), indexG, n)
        where
                dataTuple = sortBy (comparing ((!!n) .valuesR)) x
                a = map (giniWeighted dataTuple) [1..(length  (map clasR (dataTuple)) - 1)]
                indexG = findSmallestIdx a (Prelude.minimum a)

-- returns index depending on smallest gini weighted value
findSmallestIdx :: [Double] -> Double -> Int
findSmallestIdx [] _ = 0
findSmallestIdx (x:xs) number
        | x == number = 0
        | otherwise = 1 + findSmallestIdx(xs) number

-- weighted gini index depending on number of values in list
giniWeighted :: [TestData] -> Int -> Double
giniWeighted dataTuple n = wGiniHalf!!0 + wGiniHalf!!1
        where
                --first and second are two parts
                first = (take n dataTuple)
                second = (drop n dataTuple)     
                clasV = (map clasR (dataTuple)) -- jen tridy
                --using giniHalf for calculation of real weighted value from both separated parts
                wGiniHalf =  [(fromIntegral (classLenght x)/ fromIntegral (length clasV))*giniHalf x | x <- [first, second] ]

-- help function for getting lenght just for making the code more transparent
classLenght :: [TestData] -> Int
classLenght x = length (map clasR x)

-- calculating gini index in two separated parts 
-- gini index formula (1- giniIndex)
giniHalf ::  [TestData] -> Double
giniHalf half = 1- (giniIndex clasTypesCountHalf (fromIntegral(length clasHalf)))
        where
                clasHalf = (map clasR half) -- jen tridy
                -- clasTypesHalf = removeDuplicates clasHalf -- vypis druhu trid
                clasTypesCountHalf = (map (countClas clasHalf) (removeDuplicates clasHalf) ) -- vypis poctu jednotlivych trid

-- one part of gini index sum(px^2)
giniIndex :: [Int] -> Int -> Double
giniIndex [] _ = 0
giniIndex (x:xs) n = (((fromIntegral x)/(fromIntegral n))**2) + giniIndex xs n

--count of one class in list
--this is for calculating gini index
countClas :: [String] -> String-> Int
countClas [] _ = 0
countClas (x:xs) clasName
        | clasName == x = 1 + countClas xs clasName
        | otherwise = countClas xs clasName


--isInList and removeDuplicates are taken from flp exercises
--finds if value is in list - class string is in list?
isInList :: (Eq a) => a -> [a] -> Bool
isInList _ [] = False
isInList a (x:xs)
        | a == x = True
        | otherwise = isInList a xs

--remove duplicates from list of classes
--this is for controlling if in one part is only one class to print loaf
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [a] = [a]
removeDuplicates (x:xs)
        | isInList x xs = removeDuplicates xs
        | otherwise = x:(removeDuplicates xs)

-- returning average of list of values
giveAverage :: [[Double]] -> [Double]
giveAverage [] = []
giveAverage [x] = x
giveAverage (x:s:xs) = (( (+) (head x) (head s)) /2 ) : giveAverage (s:xs)

-- parse input into TreeData structure
parseToStruct :: [Char] -> TestData
parseToStruct x = (TestData className numbers)
        where 
                line = splitOn "," x
                len = length line
                numbers = (map read (take (len-1) line)::[Double]) --taking all elements except class
                className = (line !! (len-1)) --taking last element of line in output, that contains class

