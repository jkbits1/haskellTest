module Zip where

-- import Fac
-- import System.Console.Haskeline

fac :: Int -> Int
fac 0 = 1
fac n = fac (n-1) * n

facC :: Char
facC = 'a'

zipp :: String -> String -> String
zipp [] _ = []
zipp _ [] = []
zipp as bs = head as : head bs : [] ++ zipp (tail as) (tail bs)

mainX = do
    input1 <- getLine :: IO String
    input2 <- getLine :: IO String
    putStrLn $ zipp input1 input2



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