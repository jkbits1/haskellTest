module Perm where

-- import Fac
-- import System.Console.Haskeline

strPermPart :: String -> String
strPermPart [] = []
strPermPart s = (head $ tail s) : head s : []

strPerm [] = []
strPerm s = (strPermPart $ take 2 s) ++ (strPermPart $ drop 2 s)

getLines :: Int -> IO [String]
getLines n = sequence $ replicate n getLine

--mainX = do
--    input1 <- getLine :: IO String
--    input2 <- getLine :: IO String
--    putStrLn $ zipp input1 input2

mainX :: IO ()
mainX =
  do
    putStrLn "Continue?"
    response <- getYN
    if response == True
      then
        mainX
      else
        return ()
    
    

getYN :: IO Bool
getYN = 
  do 
    answer <- getLine
    if answer == "y"
      then
        return True
      else
        return False
        


-- pt :: Int -> Int -> Int
-- pt row col = (fac row) `div` ((fac col) * fac(row -col))


-- main' = do
    -- input <- readLine :: IO Int
    -- input <- getLine -- :: IO Int
    -- print . pt $ read input

-- https://www.hackerrank.com/challenges/fp-filter-array

    -- input <- getLine
    -- print . pt . (read :: String -> Int) $ input

-- from fib challenge
-- main = do
    -- input <- getLine
    -- print . fib . (read :: String -> Int) $ input