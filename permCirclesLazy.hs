import Control.DeepSeq

import Data.List
import Numeric

-- for profiling
import System.Environment
import Text.Printf

--import Control.DeepSeq

{-# LANGUAGE BangPatterns #-}


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
--first =     [2,  3,  4,  1]
--second =    [5,  6,  7,  8]
--three =     [9,  10, 11, 12]
--
--answers =   [15, 18, 21, 24]
--getCounterBase = 24

--first =     [ 1,  2,  3,  4,  5]
--first =     [ 2,  3,  4,  5,  1]
--second =    [ 6,  7,  8,  9, 10]
--three =     [11, 12, 13, 14, 15]
--
--answers =   [18, 21, 24, 27, 30]
--getCounterBase = 120

--first =     [ 1,  2,  3,  4,  5,  6]
--first =     [ 2,  3,  4,  5,  6,  1]
--second =    [ 7,  8,  9, 10, 11, 12]
--three =     [13, 14, 15, 16, 17, 18]
--answers =   [21, 24, 27, 30, 33, 36]
--getCounterBase = 720

--first =     [1,   2,  3,  4,  5,  6,  7]
first =     [7,   1,  2,  3,  4,  5,  6]
second =    [8,   9, 10, 11, 12, 13, 14]
three =     [15, 16, 17, 18, 19, 20, 21]

answers =   [24, 27, 30, 33, 36, 39, 42] -- for 1..7
--answers =   [25, 28, 31, 34, 37, 40, 36] -- for first as below
--first =     [ 2,  3,  4,  5,  6,  7,  1]
getCounterBase = 5040


--first =     [6, 5, 5, 6, 5, 4, 5, 4]
--second =    [4, 2, 2, 2, 4, 3, 3, 1]
--three =     [1, 3, 2, 3, 3, 2, 4, 3]
--
--answers = [12, 8, 12, 10, 10, 12, 10, 8]

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
-- ghc --make -O2 permCirclesLazy.hs -rtsopts
-- ghc --make -O2 ./permCirclesLazy.hs -prof
-- -auto-all -caf-all -fforce-recomp
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
main = methodChoice

methodChoice :: IO ()
methodChoice =
  let
    results choice =
      case choice of
--        "0" ->        show $ findAnswerLazy2a
        "0" ->        show $ findAnswerLazy2
        "1" ->        show $ head findAnswerLazy3
        "2" ->        show $ head findAnswerLazy3a
        "3" ->        show $ head findAnswerLazy3b
        "4" ->        show $ findAnswerLazy4 lazy2startPos
        "5" ->        show $ findSpecificAnswer
        "6" ->        show $ findSpecificAnswerX
--        "2" ->        show $ findSpecificAnswer
--        otherwise ->  show $ head findSpecificAnswerPlusList
        otherwise ->  show $ head $ fst $ head findSpecificAnswerPlusList
  in
    do
      putStrLn
        ("0 - findAnswerLazy2, 1 - findAnswerLazy3, 2 - findAnswerLazy3a, 3 - findAnswerLazy3b" ++
        ", 4 - findAnswerLazy4, 5 - findSpecificAnswer, 6 - findSpecificAnswerX" ++
        ", 5+ - findSpecificAnswerPlusList")
      input1 <- getLine :: IO String
      putStrLn input1
      putStrLn $ results input1

--  --    input2 <- getLine :: IO String
--  --    putStrLn $ show $ dropInt2 1 -- zipp input1 input2
--  --    putStrLn $ show $ secLoop
--  --    putStrLn $ show $ twoWheelPerms
--  --    putStrLn $ show $ threeWheelPerms
--  --    putStrLn $ show $ findSpecificAnswer
--  --    putStrLn $ show $ head findSpecificAnswerPlusList
--  --    putStrLn $ show $ findAnswerLazy3
--  --    putStrLn $ show $ findAnswerLazy2a
--  --    putStrLn $ show $ findAnswerLazy4 0
--  --    putStrLn $ show $ findAnswerLazyBit
--  --    putStrLn $ show $ head $ findAnswerLazyBit
--  --    putStrLn $ show $ length $ findAnswerLazyBit
--  --    putStrLn $ show $ length findAnswerLazyBitSum
--  --    putStrLn $ show $ head findAnswerLazyBitSum
--  --    putStrLn $ show $ findAnswerLazyBitSum
--  --    putStrLn $ show $ findAnswerLazy4  83000 --0
--  --    putStrLn $ show $ findAnswerLazy4  84500 --0
--  --    putStrLn $ show $ findAnswerLazy4  80000 --0
--  --    putStrLn $ show $ findAnswerLazy4  86399 --0
--  --    putStrLn $ show $ findAnswerLazy4  86400 --0
--  --    putStrLn $ show $ head findAnswerLazy
--  --    putStrLn $ show $ getWheelsPermAnswers 1
--
--
-- use these settings for load testing

--secLoop = permutations second
--thrLoop = permutations three
--ansLoop = permutations answers
wheelLoopFromStartPos pos = permutations pos
--lazy2startPos = 83000
--lazy2startPos = 0


mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)


turnWheel :: WheelPosition -> Int -> WheelPosition
turnWheel wheel chunk = (drop chunk wheel) ++ (take chunk wheel)

buildWheelLoop :: [WheelPosition] -> WheelPosition -> Int -> WheelLoop
buildWheelLoop positions pos 0 = positions ++ [pos]
buildWheelLoop positions pos count = buildWheelLoop (positions ++ [turnWheel pos count]) pos (count-1)

wheelLoopFromStartPos :: WheelPosition -> WheelLoop
--wheelLoopFromStartPos pos = buildWheelLoop [] pos $ (length pos) - 1

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
--findSpecificAnswer :: [(LoopsPermAnswers, LoopsPermutation)]
findSpecificAnswer :: (LoopsPermAnswers, LoopsPermutation)
findSpecificAnswer =
  head $
    dropWhile (\(answer, _) -> (elem answer ansLoop) == False) answersPlusList
--    filter (\(answer, _) -> elem answer ansLoop) answersPlusList

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



twoWheelPermsIndexed :: [a] -> [(Int, a)]
twoWheelPermsIndexed perms = zip [1..] perms

twoWheelPermsAdded :: [[Int]] -> [Int]
twoWheelPermsAdded items = zipWith (+) (head items) (head $ drop 1 items)

mergeTwoWheelPerms :: [[Int]]
mergeTwoWheelPerms = map twoWheelPermsAdded twoWheelPerms

indexedResults :: [(Int, [Int])]
indexedResults = twoWheelPermsIndexed mergeTwoWheelPerms


--appendTwoWheelPermsX :: WheelPosition -> [LoopsPermutation]
appendTwoWheelPermsX :: [Int] -> [(Int, [Int])]
appendTwoWheelPermsX thrPos =
  map (\(i, twoLoopResults) ->  (i, zipWith (+) twoLoopResults thrPos)) indexedResults
-- param = [7,8,9]
-- xs = [[Int]]
--[ [[1,2,3],[4,5,6],[7,8,9]], [[1,2,3],[5,4,6],[7,8,9]] ...]

threeWheelPermsX :: [(Int, [Int])]
threeWheelPermsX = concat $ map appendTwoWheelPermsX thrLoop

--findSpecificAnswerX :: [(LoopsPermAnswers, LoopsPermutation)]
findSpecificAnswerX =
  let
    (answerIndex, correctAnswer) =
      head $ filter (\(idx, answer) -> elem answer ansLoop) threeWheelPermsX
  in
    threeWheelsPermsItemByCounter $ getCounter answerIndex


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

--getCounterBase = 8
lazy2startPos = 0

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
--          0 -> (x, [0,0,0])
--          1 -> (x, [0,0] ++ xs)
--          2 -> (x, [0] ++ xs)
          0 -> (x, [0,0])
          1 -> (x, [0] ++ xs)
          otherwise -> (x, xs)

wheelPermsItem :: Int -> WheelLoop -> WheelPosition
--wheelPermsItem idx loop = head $ drop idx loop
wheelPermsItem idx loop = loop !! idx

threeWheelsPermsItemByCounterTest :: (a, Counter) -> (Int, Int)
threeWheelsPermsItemByCounterTest (_, counter) =
  let
    sec_idx = head counter
    thr_idx = head $ drop 1 counter
--    thr_idx = counter !! 1
    -- ans_idx = head $ drop 2 counter
   in
    (sec_idx, thr_idx)


threeWheelsPermsItemByCounter :: (a, Counter) -> LoopsPermutation
threeWheelsPermsItemByCounter (_, counter) =
  let
    sec_idx = head counter
    thr_idx = head $ drop 1 counter
--    thr_idx = counter !! 1
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
--    sec = xxs !! 1
--    thr = xxs !! 2
  in
    zip3 inn sec thr

getWheelsPermAnswers :: Int -> LoopsPermAnswers
getWheelsPermAnswers n =
  mapTest sumTriple $ wheelsTuple $ threeWheelsPermsItemByCounter $ getCounter n

--experiments


findAnswerTest = [ i |
--  i <- [1..5000000],
  i <- [1..(getCounterBase*getCounterBase)],
--  i <- [1..(10000000)],
--  i <- [1..(1000000)],
--  i <- [1..(getCounterBase*getCounterBase*getCounterBase)],
--  i <- [1..(getCounterBase*getCounterBase*10)],
  let ans = elem (head (getWheelsPermAnswers i)) answers, ans == True]

findAnswerTest2 =
  let
    perms x = getWheelsPermAnswers x
  in
    deepseq
      perms
      [ i |
      --  i <- [1..5000000],
        i <- [1..(getCounterBase*getCounterBase)],
--        i <- [1..(1000000)],
--        i <- [1..(10000000)],
      --  i <- [1..(getCounterBase*getCounterBase*getCounterBase)],
      --  i <- [1..(getCounterBase*getCounterBase*10)],
        let ans = elem (head (perms i)) answers, ans == True]

findAnswerTestX =
  map (\(idx, _) -> idx)
    $ filter (\(_, answer) -> elem (head answer) answers) threeWheelPermsX

mapTest f xs = foldr (\x ys -> f x : ys) [] xs

--findAnswerLazyBit = [ c | i <- [1..84600], let c = getCounter i]
--findAnswerLazyBit = [ item | i <- [1..84600], let item = threeWheelsPermsItemByCounter $ getCounter i]
findAnswerLazyBit = [ item | i <- [0..84600], let item = wheelsTuple $ threeWheelsPermsItemByCounter $ getCounter i]

findAnswerLazyBitSum = map sumTriple $ concat findAnswerLazyBit

-- elem (getWheelsPermAnswers 120) ansLoop

--NOTE: Are probs above issues of stack or memory?

--NOTE: type of stack-efficient solutions - whether tail recursion etc
-- NOTE: efficient, but not lazy - fn doesn't stop at the answer, but won't blow out stack
-- NOTE <- is a generator
--findAnswerLazy = [ i | i <- [1..512], let ans = elem (getWheelsPermAnswers i) ansLoop, ans == True]
findAnswerLazy = [ i | i <- [0..], let ans = elem (getWheelsPermAnswers i) ansLoop, ans == True]

-- NOTE: Lazy
-- gets first true result, but otherwise has no stopping condition
findAnswerLazy2 =
  take 1 [
    i |
      i <- [lazy2startPos..], let ans = elem (getWheelsPermAnswers i) ansLoop, ans == True]

findAnswerLazy2a =
  let
    permAnswers i = (getWheelsPermAnswers i)
  in
--    seq
    deepseq
      permAnswers
      take 1 [
        i |
          i <- [lazy2startPos..], let ans = elem (permAnswers i) ansLoop, ans == True]

-- this is list comprehension rewritten, and is efficient. however, it does go through
-- entire sets of perms several times NOTE: MAYBE NOT, see fns below
-- ?? how to rewrite this in a lazy eval method? does a take x at each stage help? or not needed
--      may have to merge stages etc
-- ?? how to rewrite list comp in a lazy eval method?
findAnswerLazy3 =
  let
    ansH =
      fst $
      head $
--     map (\(i, _) -> i) $
--          filter (\(i, b) -> b == True)
          dropWhile (\(_, b) -> b == False)
            $ map (\i -> (i, elem (getWheelsPermAnswers i) ansLoop))
--              [1..512]
              [1..]
--                                                                  NOTE: [1..] works too,
--                                                                        proves is lazy eval
   in
    threeWheelsPermsItemByCounter $ getCounter ansH

findAnswerLazy3a =
  let
    ansH =
      fst $
      head $
--     map (\(i, _) -> i) $
--          filter (\(i, b) -> b == True)
          dropWhile (\(_, b) -> b == False)
            $ map (\i -> (i, elem (getWheelsPermAnswers i) ansLoop))
--              [1..512]
--              [1..]
              findAnswerTest
--                                                                  NOTE: [1..] works too,
--                                                                        proves is lazy eval
   in
    threeWheelsPermsItemByCounter $ getCounter ansH

findAnswerLazy3b =
  let
    ansH =
      fst $
      head $
        dropWhile (\(_, b) -> b == False)
          $ map (\i -> (i, elem (getWheelsPermAnswers i) ansLoop))
            findAnswerTestX
   in
    threeWheelsPermsItemByCounter $ getCounter ansH


findAnswerLazy4 i =
  let
    item = wheelsTuple $ threeWheelsPermsItemByCounter $ getCounter i
    ans = map sumTriple item
    found = elem ans ansLoop
    processFound found =
      case found of
        True -> i
        False -> findAnswerLazy4 $ i + 1
  --    found
  in
    deepseq
      found
      processFound found

findAnswerLazy5 start =
  let
    checked = findAnswerTest
    findAnswerLazy4a i checked =
      case elem i checked of
        False -> findAnswerLazy4a (i + 1) checked
        True ->
          let
            item = wheelsTuple $ threeWheelsPermsItemByCounter $ getCounter i
            ans = map sumTriple item
            found = elem ans ansLoop
            processFound found =
              case found of
                True -> i
                False -> findAnswerLazy4a (i + 1) checked
          --    found
          in
            deepseq
              found
              processFound found
  in
    findAnswerLazy4a start checked



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


--NOTE ideas,
--filter second third etc
--filter duplicates, can we predict, eg from twoPerms ?
--rotate candidates
-- getCounter, can it provide hints on twoWheelPerms change points etc?
--  consider what it genrates, ...

