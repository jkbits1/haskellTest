
import Data.List

-- test data that is easier to visualise and debug
--inner =     [1,2,3]
--second =    [4,5,6]
--three =     [7,8,9]

inner =     [6, 5, 5, 6, 5, 4, 5, 4]
second =    [4, 2, 2, 2, 4, 3, 3, 1]
three =     [1, 3, 2, 3, 3, 2, 4, 3]

answers = [12, 8, 12, 10, 10, 12, 10, 8]

listLoopItem list chunk = (drop chunk list) ++ (take chunk list)

listLoops :: [[Int]] -> [Int] -> Int -> [[Int]]
listLoops lists seed 0 = lists ++ [seed]
listLoops lists seed count = listLoops (lists ++ [listLoopItem seed count]) seed (count-1)

wheelPerms :: [Int] -> [[Int]]
wheelPerms xs = listLoops [] xs $ (length xs) - 1

secPerms :: [[Int]]
secPerms = wheelPerms second
-- [ [4,5,6], [5,6,4] ... ]

thrPerms :: [[Int]]
thrPerms = wheelPerms three
-- [ [7,8,9], [8,9,7] ... ]

twoListPerms :: [[[Int]]]
twoListPerms = map (\sec -> inner : sec : []) secPerms
--twoListPerms = map attachInnerList secPerms
-- result - [ [[1,2,3], [4,5,6]],
--            [[1,2,3], [5,6,4]], ...]

--addTuple :: (Int, Int) -> Int
--addTuple (a, b) = a + b
minusTuple :: (Int, Int) -> Int
minusTuple  (a, b) = b - a

appendTwoListPerms :: [Int] -> [[[Int]]]
appendTwoListPerms thr = map (\xs ->  xs ++ [thr]) twoListPerms
-- param = [7,8,9]
-- xs = [[Int]]
--[ [[1,2,3],[4,5,6],[7,8,9]], [[1,2,3],[5,4,6],[7,8,9]] ...]

threeListPerms :: [[[Int]]]
threeListPerms = concat $ map appendTwoListPerms thrPerms
-- result -
-- [ [[1,2,3],[4,5,6],[7,8,9]],  [[1,2,3],[5,4,6],[7,8,9]] ...,
--   [[1,2,3],[4,5,6],[8,7,9]],  [[1,2,3],[5,4,6],[8,7,9]] ... ]

sumTriple :: (Int, Int, Int) -> Int
sumTriple (a, b, c) = a + b + c

tuplesFromLists :: [[Int]] -> [(Int, Int, Int)]
tuplesFromLists lists =
    let list1   = head lists
        list2   = head $ drop 1 lists
        list3   = head $ drop 2 lists
    in
        zip3 list1 list2 list3

sumPlusLists :: [[Int]] -> [([Int], [[Int]])]
sumPlusLists lists = [(map sumTriple $ tuplesFromLists lists, lists)]

answersPlusList :: [([Int], [[Int]])]
answersPlusList = concat $ map sumPlusLists threeListPerms

answersPermsLoop2 :: ([Int], t) -> ([[Int]], t)
answersPermsLoop2 (ans, lists) = (wheelPerms ans, lists)

answersPermsPlusList :: [([[Int]], [[Int]])]
answersPermsPlusList = map answersPermsLoop2 answersPlusList

-- finds solution
findSpecificAnswerPlusList :: [([[Int]], [[Int]])]
findSpecificAnswerPlusList =
    filter (\(ans, lists) -> elem answers ans) answersPermsPlusList

-- display solution
displaySpecificAnswers =
    snd $ head findSpecificAnswerPlusList



--
--PREPARATORY WORK
--

-- correct functions, refactored out

--attachLists :: [Int] -> [Int] -> [[Int]]
--attachLists inner second = inner : second : []
-- params - [1,2,3] [4,5,6]
-- result - [ [1,2,3], [4,5,6] ]

--attachInnerList :: [Int] -> [[Int]]
--attachInnerList = attachLists inner
-- param - [4,5,6]
-- result - [ [1,2,3], [4,5,6] ]




-- these functions confirm whether the solution can be found, but do not show which items
-- create the solution

--sumLists :: [[Int]] -> [Int]
--sumLists lists = map sumTriple $ tuplesFromLists lists
--
--answersList :: [[Int]]
--answersList = map sumLists createThreeListPerms
--
--answersPermsLoop :: [Int] -> [[Int]]
--answersPermsLoop xs = wheelPerms xs
--
--answersPerms :: [[[Int]]]
--answersPerms = map answersPermsLoop answersList
--
--findSpecificAnswer :: [[Int]]
--findSpecificAnswer =
--    filter (\xs -> xs == answers) $ concat $ answersPerms



-- given two wheels perms and actual answer, create list of diffs
-- create perms of last wheel, then iterate through diffs for a match

--sumTwoLists :: [[Int]] -> [Int]
--sumTwoLists lists =
--    let list1   = head lists
--        list2   = head $ drop 1 lists
--        tups    = zip list1 list2
--    in
--        map (\(x, y) -> x + y) tups

--summedTwoLists :: [[Int]]
--summedTwoLists = map sumTwoLists twoListPerms

--createCandidate list =
--    let tuples = zip list answers
--    in
--        map (\t -> minusTuple t) tuples
--
--candidates = map createCandidate summedTwoLists




-- initial practise with strings, as easier to visualise nested lists

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

