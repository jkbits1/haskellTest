
import Data.List
import Numeric

-- for profiling
import System.Environment
import Text.Printf

-- test data that is easier to visualise and debug
--first =     [1,2,3]
--second =    [4,5,6]
--three =     [7,8,9]
--
--answers = [12, 15, 18]

--
-- assorted load test values
--

--first =     [1,  2,  3,  4]
--second =    [5,  6,  7,  8]
--three =     [9,  10, 11, 12]
--
--answers =   [15, 18, 21, 24]

--first =     [1,   2,  3,  4,  5,  6]
--second =    [7,   8,  9, 10, 11, 12]
--three =     [13, 14, 15, 16, 17, 18]

--answers =   [21, 24, 27, 30, 33, 36] -- for 1..6
--answers =   [26, 24, 27, 30, 33, 31]   -- for first as below
--first =     [6,   2,  3,  4,  5,  1]


first =     [6, 5, 5, 6, 5, 4, 5, 4]
second =    [4, 2, 2, 2, 4, 3, 3, 1]
three =     [1, 3, 2, 3, 3, 2, 4, 3]

answers = [12, 8, 12, 10, 10, 12, 10, 8]

type WheelPosition    = [Int]
type WheelLoop        = [WheelPosition]
type LoopsPermutation = [WheelPosition]
type LoopsPermColumn  = (Int, Int, Int)
type LoopsPermAnswers = [Int]
type Counter          = [Int]

dropInt :: Int -> WheelPosition -> Int -> WheelPosition
dropInt n pos turns = turnWheel pos turns

dropInt2 :: Int -> [LoopsPermutation]
dropInt2 n = twoWheelPerms

dropInt2a :: Int -> WheelLoop
dropInt2a n = secLoop

-- for profiling
--ghc --make -O2 permCirclesLazy.hs
--ghc -O2 --make ./permCirclesLazy.hs -prof
---auto-all -caf-all -fforce-recomp
-- -rtsopts
--Measure-Command {.\permCirclesLazy.exe 1 +RTS -sstderr -hc -i0 -p}
--main :: IO ()
--main = do
--        [d] <- map read `fmap` getArgs
--------        w <- turnWheel first 1
------        printf "%d" $ head (turnWheel first d)
------        printf "%d" $ head (dropInt d first d)
--        printf "%d" $ head (head (head (dropInt2 d)))
----        printf "%d" $ head (head (dropInt2a d))
------        printf "%d" head $ head $ head $ (dropInt d)
------        printf "%d" $ head $ (\d -> twoWheelPerms) d
------        printf "%d" $ (\v -> 1.0) d
------        printf "%d\n" (mean [1..d])
------        printf "%f\n" (mean [1..d])

--main :: IO ()
--main = do
--        [d] <- ((map read) `fmap`) getArgs
--        printf "%f\n" (mean [1..d])

main :: IO ()
main = do
    input1 <- getLine :: IO String
    input2 <- getLine :: IO String
--    putStrLn $ show $ dropInt2 1 -- zipp input1 input2
--    putStrLn $ show $ secLoop
--    putStrLn $ show $ twoWheelPerms
--    putStrLn $ show $ threeWheelPerms
--    putStrLn $ show $ findSpecificAnswer
    putStrLn $ show $ head findSpecificAnswerPlusList
--    putStrLn $ show $ findAnswerLazy3


--
-- use these settings for load testing
--
--secLoop = permutations second
--thrLoop = permutations three
--ansLoop = permutations answers
--getCounterBase = 720


mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)


turnWheel :: WheelPosition -> Int -> WheelPosition
turnWheel wheel chunk = (drop chunk wheel) ++ (take chunk wheel)

buildWheelLoop :: [WheelPosition] -> WheelPosition -> Int -> WheelLoop
buildWheelLoop positions pos 0 = positions ++ [pos]
buildWheelLoop positions pos count = buildWheelLoop (positions ++ [turnWheel pos count]) pos (count-1)

wheelLoopFromStartPos :: WheelPosition -> WheelLoop
wheelLoopFromStartPos pos = buildWheelLoop [] pos $ (length pos) - 1

secLoop :: WheelLoop
secLoop = wheelLoopFromStartPos second
-- [ [4,5,6], [5,6,4] ... ]

thrLoop :: WheelLoop
thrLoop = wheelLoopFromStartPos three
-- [ [7,8,9], [8,9,7] ... ]

-- NOTE - used for revised first solution and lazy eval solution
ansLoop :: WheelLoop
ansLoop = wheelLoopFromStartPos answers

twoWheelPerms :: [LoopsPermutation]
twoWheelPerms = map (\secPos -> first : secPos : []) secLoop
--twoWheelPerms = map attachInnerList secLoop
-- result - [ [[1,2,3], [4,5,6]],
--            [[1,2,3], [5,6,4]], ...]

--addTuple :: (Int, Int) -> Int
--addTuple (a, b) = a + b
minusTuple :: (Int, Int) -> Int
minusTuple  (a, b) = b - a

-- could refactor as a let clause, but then lose types
appendTwoWheelPerms :: WheelPosition -> [LoopsPermutation]
appendTwoWheelPerms thrPos =
  map (\twoLoopPerm ->  twoLoopPerm ++ [thrPos]) twoWheelPerms
-- param = [7,8,9]
-- xs = [[Int]]
--[ [[1,2,3],[4,5,6],[7,8,9]], [[1,2,3],[5,4,6],[7,8,9]] ...]

threeWheelPerms :: [LoopsPermutation]
threeWheelPerms = concat $ map appendTwoWheelPerms thrLoop
-- result -
-- [ [[1,2,3],[4,5,6],[7,8,9]],  [[1,2,3],[5,4,6],[7,8,9]] ...,
--   [[1,2,3],[4,5,6],[8,7,9]],  [[1,2,3],[5,4,6],[8,7,9]] ... ]

sumTriple :: LoopsPermColumn -> Int
sumTriple (a, b, c) = a + b + c

columnsFromPerm :: LoopsPermutation -> [LoopsPermColumn]
columnsFromPerm perm =
    let firstPos  = head perm
        secPos    = head $ drop 1 perm
        thrPos    = head $ drop 2 perm
    in
        zip3 firstPos secPos thrPos

sumPlusPerm :: LoopsPermutation -> [(LoopsPermAnswers, LoopsPermutation)]
sumPlusPerm perm = [(map sumTriple $ columnsFromPerm perm, perm)]

answersPlusList :: [(LoopsPermAnswers, LoopsPermutation)]
answersPlusList = concat $ map sumPlusPerm threeWheelPerms

--NOTE: SOLUTION REFACTOR - added this step after created lazy eval solution
findSpecificAnswer :: [(LoopsPermAnswers, LoopsPermutation)]
findSpecificAnswer =
    filter (\(answer, _) -> elem answer ansLoop) answersPlusList

answersPermsLoop2 :: ([Int], t) -> ([[Int]], t)
answersPermsLoop2 (ans, lists) = (wheelLoopFromStartPos ans, lists)

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
initCounter :: Counter
initCounter = [0,0,0]

-- incrementCounter = [0,0,0]

getCounterBase = 8

getCounter :: Int -> (Int, Counter)
getCounter x =
--  case x >= 512 of
  case x < 0 of
    True -> getCounter 0
    False ->
      let
        xs = digits getCounterBase x
      in
        case length xs of
          0 -> (x, [0,0,0])
          1 -> (x, [0,0] ++ xs)
          2 -> (x, [0] ++ xs)
          otherwise -> (x, xs)


wheelPermsItem :: Int -> WheelLoop -> WheelPosition
wheelPermsItem idx loop = head $ drop idx loop

threeWheelsPermsItemByCounter :: (a, Counter) -> LoopsPermutation
threeWheelsPermsItemByCounter (_, counter) =
  let
    sec_idx = head counter
    thr_idx = head $ drop 1 counter
    -- ans_idx = head $ drop 2 counter
   in
    [first]
    ++
      [wheelPermsItem sec_idx secLoop] ++
      [wheelPermsItem thr_idx thrLoop]
--      ++ [wheelPermsItem ans_idx ansLoop]

wheelsTuple :: LoopsPermutation -> [LoopsPermColumn]
wheelsTuple xxs =
  let
    inn = head xxs
    sec = head $ drop 1 xxs
    thr = head $ drop 2 xxs
  in
    zip3 inn sec thr

getWheelsPermAnswers :: Int -> LoopsPermAnswers
getWheelsPermAnswers n =
  map sumTriple $ wheelsTuple $ threeWheelsPermsItemByCounter $ getCounter n


-- elem (getWheelsPermAnswers 120) ansLoop

--NOTE: Are probs above issues of stack or memory?

--NOTE: type of stack-efficient solutions - whether tail recursion etc
-- NOTE: efficient, but not lazy - fn doesn't stop at the answer, but won't blow out stack
-- NOTE <- is a generator
findAnswerLazy = [ i | i <- [1..512], let ans = elem (getWheelsPermAnswers i) ansLoop, ans == True]

-- NOTE: Lazy
-- gets first true result, but otherwise has no stopping condition
findAnswerLazy2 = take 1 [ i | i <- [1..], let ans = elem (getWheelsPermAnswers i) ansLoop, ans == True]

-- this is list comprehension rewritten, and is efficient. however, it does go through
-- entire sets of perms several times NOTE: MAYBE NOT, see fns below
-- ?? how to rewrite this in a lazy eval method? does a take x at each stage help? or not needed
--      may have to merge stages etc
-- ?? how to rewrite list comp in a lazy eval method?
findAnswerLazy3 =
  let
    ansH = head $
     map (\(i, _) -> i) $
          filter (\(i, b) -> b == True) $
             map (\i -> (i, elem (getWheelsPermAnswers i) ansLoop))
--           [1..512]
             [1..]
--                                                                  NOTE: [1..] works too,
--                                                                        proves is lazy eval
   in
    threeWheelsPermsItemByCounter $ getCounter ansH

-- expected this to work
testInf0 = [1..]
testLazy0 = take 1 $ testInf0

-- wasn't sure, but this worked
testInf = map (\x -> x+ 1) [1..]
testLazy = take 1 $ testInf

--didn't expect this to work, but it does
testInf2 = filter (\y -> y > 5) $ map (\x -> x+ 1) [1..]
testLazy2 = take 1 $ testInf2
testLazy2a = head $ testInf2


--tail-recursion version




--
--PREPARATORY WORK
--

-- correct functions, refactored out

--attachLists :: [Int] -> [Int] -> [[Int]]
--attachLists first second = first : second : []
-- params - [1,2,3] [4,5,6]
-- result - [ [1,2,3], [4,5,6] ]

--attachInnerList :: [Int] -> [[Int]]
--attachInnerList = attachLists first
-- param - [4,5,6]
-- result - [ [1,2,3], [4,5,6] ]




-- these functions confirm whether the solution can be found, but do not show which items
-- create the solution

--sumLists :: [[Int]] -> [Int]
--sumLists lists = map sumTriple $ columnsFromPerm lists
--
--answersList :: [[Int]]
--answersList = map sumLists createThreeListPerms
--
--answersPermsLoop :: [Int] -> [[Int]]
--answersPermsLoop xs = wheelLoopFromStartPos xs
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
--summedTwoLists = map sumTwoLists twoWheelPerms

--createCandidate list =
--    let tuples = zip list answers
--    in
--        map (\t -> minusTuple t) tuples
--
--candidates = map createCandidate summedTwoLists




