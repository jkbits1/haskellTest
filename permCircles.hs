
import Data.List

--inner =     [1,2,3]
--second =    [4,5,6]
--three =     [7,8,9]

inner =     [6, 5, 5, 6, 5, 4, 5, 4]
second =    [4, 2, 2, 2, 4, 3, 3, 1]
three =     [1, 3, 2, 3, 3, 2, 4, 3]

answers = [12, 8, 12, 10, 10, 12, 10, 8]

listLoopItem list chunk = (drop chunk list) ++ (take chunk list)

createListLoops :: [[Int]] -> [Int] -> Int -> [[Int]]
createListLoops lists seed 0 = lists ++ [seed]
createListLoops lists seed count =
    createListLoops
        (lists ++ [(listLoopItem seed count)])
        seed (count-1)

wheelPerms :: [Int] -> [[Int]]
--wheelPerms xs = permutations xs
wheelPerms xs = createListLoops [] xs $ (length xs) - 1

secPerms :: [[Int]]
secPerms = wheelPerms second
-- [ [4,5,6], [5,4,6] ... ]

thrPerms :: [[Int]]
thrPerms = wheelPerms three

attachLists :: [Int] -> [Int] -> [[Int]]
attachLists inner second = inner : second : []
-- params - [1,2,3] [4,5,6]
-- result - [ [1,2,3], [4,5,6] ]

attachInnerList :: [Int] -> [[Int]]
attachInnerList = attachLists inner
-- param - [4,5,6]
-- result - [ [1,2,3], [4,5,6] ]

createTwoListPerms :: [[[Int]]]
createTwoListPerms = map attachInnerList secPerms
-- result - [ [[1,2,3], [4,5,6]],
--            [[1,2,3], [5,6,4]], ...]

-- given two wheels perms and actual answer, create list of diffs
-- create perms of last wheel, then iterate through diffs for a match

sumTwoTuple :: (Int, Int) -> Int
sumTwoTuple (a, b) = a + b

sumTwoLists :: [[Int]] -> [Int]
sumTwoLists lists =
    let list1   = head lists
        list2   = head $ drop 1 lists
        tups    = zip list1 list2
    in
        map (\(x, y) -> x + y) tups

summedTwoLists :: [[Int]]
summedTwoLists = map sumTwoLists createTwoListPerms

minusTwoTuple :: (Int, Int) -> Int
minusTwoTuple  (a, b) = b - a

--candidates = map (\xs -> ) summedTwoLists

createCandidate list =
    let tuples = zip list answers
    in
--        tuples
        map (\t -> minusTwoTuple t) tuples

candidates = map createCandidate summedTwoLists

--elem
--test with default three exisitn in perms of three



appendTwoListPerms :: [Int] -> [[[Int]]]
appendTwoListPerms thr = map (\xs ->  xs ++ [thr]) createTwoListPerms
-- param = [7,8,9]
-- xs = [[Int]]
--[ [[1,2,3],[4,5,6],[7,8,9]], [[1,2,3],[5,4,6],[7,8,9]] ...]



-- !! meets answer level, but overloads memory
createThreeListPerms :: [[[Int]]]
createThreeListPerms = concat $ map appendTwoListPerms thrPerms
-- result -
--[
--[ [[1,2,3],[4,5,6],[7,8,9]],  [[1,2,3],[5,4,6],[7,8,9]] ... ],
--[ [[1,2,3],[4,5,6],[8,7,9]],  [[1,2,3],[5,4,6],[8,7,9]]  ...]
-- ...
--]

--twoRings :: [Int] -> [Int] -> [[Int], [Int]]
--twoRings inner second =

sumTuple :: (Int, Int, Int) -> Int
sumTuple (a, b, c) = a + b + c

sumLists :: [[Int]] -> [Int]
sumLists lists =
    let list1   = head lists
        list2   = head $ drop 1 lists
        list3   = head $ drop 2 lists
        tups    = zip3 list1 list2 list3
    in
        map sumTuple tups

answersList :: [[Int]]
answersList = map sumLists createThreeListPerms

answersPermsLoop :: [Int] -> [[Int]]
answersPermsLoop xs = wheelPerms xs

answersPerms :: [[[Int]]]
answersPerms = map answersPermsLoop answersList

findSpecificAnswer :: [[Int]]
findSpecificAnswer = filter (\xs -> xs == answers)
    $ concat $ answersPerms


sumPlusLists :: [[Int]] -> [([Int], [[Int]])]
sumPlusLists lists =
    let list1   = head lists
        list2   = head $ drop 1 lists
        list3   = head $ drop 2 lists
        tups    = zip3 list1 list2 list3
    in
--        map sumTuple tups
--        [(tups, lists)]
        [(map sumTuple tups, lists)]

answersPlusList :: [([Int], [[Int]])]
answersPlusList = concat $ map sumPlusLists createThreeListPerms

answersPermsLoop2 :: ([Int], t) -> ([[Int]], t)
answersPermsLoop2 (ans, lists) = (wheelPerms ans, lists)

answersPermsPlusList :: [([[Int]], [[Int]])]
answersPermsPlusList = map answersPermsLoop2 answersPlusList

-- this should find the solution
findSpecificAnswerPlusList :: [([[Int]], [[Int]])]
findSpecificAnswerPlusList =
    filter (\(ans, lists) -> elem answers ans) answersPermsPlusList



inStr = "abc"
secStr = "def"
thrStr = "ghi"

secStrPerms = permutations secStr
--["def", "edf" ...]

thrStrPerms = permutations thrStr
--["ghi", "hig" ...]

thrStrPermsList = map (\s -> [s]) thrStrPerms

attachStrs :: String -> String -> [String]
attachStrs s1 s2 = s1 : s2 : []

attachInStr :: String -> [String]
attachInStr = attachStrs inStr

createTwoStrPerms :: [[String]]
createTwoStrPerms = map attachInStr secStrPerms
-- [ ["abc", "def"], ["abc", "edf"], ...]

attachListStrs :: [String] -> [String] -> [[String]]
--attachListStrs ss1 ss2 = ss1 : ss2 : []
attachListStrs ss1 ss2 = [ss1 ++ ss2]

attachTwoStrs :: [String] -> [[String]]
attachTwoStrs = attachListStrs $ head createTwoStrPerms

createThreeStrPerms :: [[[String]]]
createThreeStrPerms = map attachTwoStrs thrStrPermsList

appendStrPerms = map (\xs ->  xs ++ ["ghi"]) createTwoStrPerms
-- [ ["abc", "def", "ghi"], ["abc", "edf", "ghi"], ...]

