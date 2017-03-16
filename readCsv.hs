import Control.Monad (liftM)
import Data.List.Split (splitOn)

fileName = "divs adjusted.csv"

showCsv = putStr =<< readFile fileName

main :: IO ()
main = showCsv

-- misc functions with do syntax
fileLines :: IO [String]
fileLines = do 
  contents <- readFile fileName
  return $ lines contents

-- ignore field headings line
dataLines :: IO [String]
dataLines = do 
  contents <- readFile fileName
  return $ drop 1 $ lines contents

dataLines' = liftM (\cs -> drop 1 $ lines cs) $ readFile fileName
dataLines'' = liftM (drop 1 . lines) $ readFile fileName

lineCount :: IO Int
lineCount = do
  ls <- fileLines 
  return $ length ls

first :: IO String
first = do
  ls <- fileLines
  return $ head ls

-- function with >>= syntax
items :: IO [String]
items =
  fileLines >>= \ls ->
    return $ drop 1 ls

-- functions with liftM
fileLines' = liftM lines $ readFile fileName
lineCount' = liftM length fileLines'
first' = liftM head fileLines
items' = liftM (drop 1) fileLines    

specificLine' :: Int -> IO String
specificLine' n = liftM (!!n) fileLines

getLineFields' n = liftM (splitOn ",") $ specificLine' n

specificLineCompanyId n =  liftM (head . drop 1) $ getLineFields' n

specificLineCoId :: Int -> IO String
specificLineCoId n = do
  line <- liftM (!!n) fileLines
  return $ head . drop 1 $ splitOn "," line

specificLineCoId' n = do
  coId <- liftM (\ls -> head . drop 1 $ splitOn "," $ (!!n) ls) fileLines
  return coId

specificLineCoId'''' n = do
  coId <- liftM (head . drop 1 . splitOn "," . (!!n)) fileLines
  return coId

lineField f line = head . drop f $ splitOn "," line
lineCoId line = head . drop 1 $ splitOn "," line

specificLineField f n xs = head . drop f $ splitOn "," $ (!!n) xs
specificLineField' fno line = head . drop fno . splitOn "," . (!!line)
specificLineField'' f n xs = lineField f $ (!!n) xs

-- doesn't compile
-- specificLineField' f n = head . drop f $ splitOn "," $ (!!n)

specLineCoId n xs = specificLineField 1 n xs

-- does compile
specLineCoId' n = specificLineField 1 n

specificLineCoId'' n = do
  coId <- liftM (specLineCoId n) fileLines
  return coId

specificLineCoId''' n = do
  liftM (specLineCoId n) fileLines >>= \coId -> 
    return coId

-- convert company id field to Int
coId' line = read $ lineCoId line :: Int

coIds = do
  -- let coId line = read $ lineCoId line :: Int
  lines <- dataLines
  -- return $ map (\line -> lineCoId line) lines 
  return $ map lineCoId lines 

-- coIds' = do
--   return $ mapM lineCoId dataLines 

coLines :: Int -> IO [String]
coLines n = do
  lines <- dataLines
  -- return $ filter (\line -> lineCoId line == "4" ) lines 
  return $ filter (\line -> n == (coId' line) ) lines 

-- ghci test code
-- liftM (\l -> read $ lineCoId l :: Int) $ specificLine' 1

-- liftM length $ coLines 29
-- liftM head $ coLines 29

-- liftM (head . drop 1) $ coLines 29
--  appendFile "./t.csv" "123"

-- liftM (appendFile "./t.csv" . head . drop 1) $ coLines 29
-- liftM (show . head . drop 1) $ coLines 29

-- (appendFile "./t.csv" . head . drop 1) ["", "xyz"]

writeCoLines n = do
  lines <- coLines n
  -- return $ map (\l -> show l) lines
  -- mapM_ (\l -> appendFile "./t.csv" l) lines
  mapM_ (appendFile ("./t" ++ (show n :: String) ++ ".csv")) lines

writeCoLine n line = do
  lines <- coLines n
  -- return $ map (\l -> show l) lines
  -- mapM_ (\l -> appendFile "./t.csv" l) lines
  (appendFile ("./t" ++ (show n :: String) ++ ".csv") . head. drop line) lines



