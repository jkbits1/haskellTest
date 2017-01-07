import Control.Monad (liftM)
import Data.List.Split (splitOn)

fileName = "divs adjusted.csv"

showCsv = putStr =<< readFile fileName

main :: IO ()
main = showCsv

-- misc functions with do syntax
fileLines :: IO [String]
fileLines = do 
  ls <- readFile fileName
  return $ lines ls

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

lineField f line = head . drop f $ splitOn "," line
lineCoId line = head . drop 1 $ splitOn "," line

specificLineField f n xs = head . drop f $ splitOn "," $ (!!n) xs
specificLineField' f n xs = lineField f $ (!!n) xs

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


coLines = do
  lines <- fileLines
  return $ filter (\line -> lineCoId line == "4" ) lines 


