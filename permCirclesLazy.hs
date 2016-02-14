
import Data.List
import Numeric

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




-- Lazy

-- | Returns the digits of a positive integer as a list, in reverse order.
--   This is slightly more efficient than in forward order.
digitsRev :: Integral n
    => n -- ^ The base to use.
    -> n -- ^ The number to convert to digit form.
    -> [n] -- ^ The digits of the number in list form, in reverse.
digitsRev base i = case i of
        0 -> []
        _ -> lastDigit : digitsRev base rest
    where (rest, lastDigit) = quotRem i base

-- | Returns the digits of a positive integer as a list.
digits :: Integral n
    => n -- ^ The base to use (typically 10).
    -> n -- ^ The number to convert to digit form.
    -> [n] -- ^ The digits of the number in list form.
digits base = reverse . digitsRev base

--counter/state (each digit up to 7)
initCounter = [0,0,0]

incrementCounter = [0,0,0]

getCounter :: Int -> (Int, [Int])
getCounter x =
  case x >= 512 of
    True -> getCounter 0
    False ->
      let
        xs = digits 8 x
      in
        case length xs of
          0 -> (x, [0,0,0])
          1 -> (x, [0,0] ++ xs)
          2 -> (x, [0] ++ xs)
          otherwise -> (x, xs)


wheelPermsItem :: Int -> [[Int]] -> [Int]
wheelPermsItem idx xs = head $ drop (idx) xs

threeWheelsPermsItemByCounter counter =
  let
    s_idx = head counter
   in
    [
      inner : [wheelPermsItem s_idx secPerms]
    ]

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




