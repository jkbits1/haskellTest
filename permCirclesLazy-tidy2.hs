import Data.List
import Numeric

-- for profiling
import System.Environment
import Text.Printf

getCounterBase = 8
lazy2startPos = 0

-- test data that is easier to visualise and debug
--first =     [1,2,3]
--second =    [4,5,6]
--three =     [7,8,9]
--
--answers = [12, 15, 18]

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

turnWheel :: WheelPosition -> Int -> WheelPosition
turnWheel wheel chunk = (drop chunk wheel) ++ (take chunk wheel)

buildWheelLoop :: [WheelPosition] -> WheelPosition -> Int -> WheelLoop
buildWheelLoop positions pos 0 = positions ++ [pos]
buildWheelLoop positions pos count =
  let
    nextPositions = (positions ++ [turnWheel pos count])
  in
    buildWheelLoop nextPositions pos (count-1)

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
-- result - [ [[1,2,3], [4,5,6]],
--            [[1,2,3], [5,6,4]], ...]

-- could refactor as a let clause, but then lose types
appendTwoWheelPerms :: WheelPosition -> [LoopsPermutation]
appendTwoWheelPerms thrPos =
  map (\twoLoopPerm ->  twoLoopPerm ++ [thrPos]) twoWheelPerms
-- param = [7,8,9]
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

findSpecificAnswer :: (LoopsPermAnswers, LoopsPermutation)
findSpecificAnswer =
  head $
    dropWhile (\(answer, _) -> (elem answer ansLoop) == False) answersPlusList


twoWheelPermsIndexed :: [a] -> [(Int, a)]
twoWheelPermsIndexed perms = zip [1..] perms

twoWheelPermsAdded :: [[Int]] -> [Int]
twoWheelPermsAdded items = zipWith (+) (head items) (head $ drop 1 items)

mergeTwoWheelPerms :: [[Int]]
mergeTwoWheelPerms = map twoWheelPermsAdded twoWheelPerms

indexedResults :: [(Int, [Int])]
indexedResults = twoWheelPermsIndexed mergeTwoWheelPerms

appendTwoWheelPermsX :: [Int] -> [(Int, [Int])]
appendTwoWheelPermsX thrPos =
  map (\(i, twoLoopResults) ->  (i, zipWith (+) twoLoopResults thrPos)) indexedResults
-- param = [7,8,9]
--[ [[1,2,3],[4,5,6],[7,8,9]], [[1,2,3],[5,4,6],[7,8,9]] ...]

threeWheelPermsX :: [(Int, [Int])]
threeWheelPermsX = concat $ map appendTwoWheelPermsX thrLoop

findSpecificAnswerX =
  let
    (answerIndex, correctAnswer) =
      head $ filter (\(idx, answer) -> elem answer ansLoop) threeWheelPermsX
  in
    threeWheelsPermsItemByCounter $ getCounter answerIndex


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


getCounter :: Int -> (Int, Counter)
getCounter x =
  case x < 0 of
    True -> getCounter 0
    False ->
      let
        xs = digits getCounterBase x
      in
        case length xs of
          0 -> (x, [0,0])
          1 -> (x, [0] ++ xs)
          otherwise -> (x, xs)

wheelPermsItem :: Int -> WheelLoop -> WheelPosition
wheelPermsItem idx loop = head $ drop idx loop

threeWheelsPermsItemByCounter :: (a, Counter) -> LoopsPermutation
threeWheelsPermsItemByCounter (_, counter) =
  let
    sec_idx = head counter
    thr_idx = head $ drop 1 counter
   in
    [first]
    ++
      [wheelPermsItem sec_idx secLoop] ++
      [wheelPermsItem thr_idx thrLoop]

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
  mapTest sumTriple $ wheelsTuple $ threeWheelsPermsItemByCounter $ getCounter n


--experiments


findAnswerTest = [ i |
  i <- [1..(getCounterBase*getCounterBase)],
  let ans = elem (head (getWheelsPermAnswers i)) answers, ans == True]

findAnswerTestX =
  map (\(idx, _) -> idx)
    $ filter (\(_, answer) -> elem (head answer) answers) threeWheelPermsX

--mapTest f xs = foldr (\x ys -> f x : ys) [] xs


-- NOTE <- is a generator

-- gets first true result, but otherwise has no stopping condition
findAnswerCS2 =
  take 1 [
    i |
      i <- [0..], let ans = elem (getWheelsPermAnswers i) ansLoop, ans == True]

-- list comprehension rewritten
findAnswerCS3 =
  let
    ansH =
      fst $
      head $
        dropWhile (\(_, b) -> b == False)
          $ map (\i -> (i, elem (getWheelsPermAnswers i) ansLoop))
            [1..]
   in
    threeWheelsPermsItemByCounter $ getCounter ansH

findAnswerCS3a =
  let
    ansH =
      fst $
      head $
        dropWhile (\(_, b) -> b == False)
          $ map (\i -> (i, elem (getWheelsPermAnswers i) ansLoop))
            findAnswerTest
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

testInf0 = [1..]
testLazy0 = take 1 $ testInf0

testInf = map (\x -> x+ 1) [1..]
testLazy = take 1 $ testInf

testInf2 = filter (\y -> y > 5) $ map (\x -> x+ 1) [1..]
testLazy2 = take 1 $ testInf2
testLazy2a = head $ testInf2


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
--first =     [7,   1,  2,  3,  4,  5,  6]
--second =    [8,   9, 10, 11, 12, 13, 14]
--three =     [15, 16, 17, 18, 19, 20, 21]

--answers =   [24, 27, 30, 33, 36, 39, 42] -- for 1..7
--answers =   [25, 28, 31, 34, 37, 40, 36] -- for first as below
--first =     [ 2,  3,  4,  5,  6,  7,  1]
--getCounterBase = 5040

-- for profiling
-- ghc --make -O2 permCirclesLazy.hs -rtsopts
-- ghc --make -O2 ./permCirclesLazy.hs -prof
-- -auto-all -caf-all -fforce-recomp
-- -rtsopts
--Measure-Command {.\permCirclesLazy.exe 1 +RTS -sstderr -hc -i0 -p}

main :: IO ()
main = methodChoice

methodChoice :: IO ()
methodChoice =
  let
    results choice =
      case choice of
        "0" ->        show $ findAnswerCS2
        "1" ->        show $ head findAnswerCS3
        "2" ->        show $ head findAnswerCS3a
        "3" ->        show $ head findAnswerCS3b
        "5" ->        show $ findSpecificAnswer
        "6" ->        show $ findSpecificAnswerX
  in
    do
      putStrLn
        ("0 - findAnswerCS2, 1 - findAnswerCS3, 2 - findAnswerCS3a, 3 - findAnswerCS3b" ++
        ", 5 - findSpecificAnswer, 6 - findSpecificAnswerX")
      input1 <- getLine :: IO String
      putStrLn input1
      putStrLn $ results input1



-- use these settings for load testing

--secLoop = permutations second
--thrLoop = permutations three
--ansLoop = permutations answers
--wheelLoopFromStartPos pos = permutations pos

