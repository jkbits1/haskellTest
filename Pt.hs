module Pt where

-- import Fac
import System.Console.Haskeline

fac :: Int -> Int
fac 0 = 1
fac n = fac (n-1) * n

pt :: Int -> Int -> Int
pt row col = (fac row) `div` ((fac col) * fac(row -col))

-- pt2 :: Int -> Int
-- pt2 row = (fac row) 
    -- `div` ((fac col) * fac(row -col))
    
ptRowLoop :: Int -> Int -> [Int]
ptRowLoop row 0 = [pt row 0]
ptRowLoop row col = 
    [pt row col] ++ ptRowLoop row (col-1)

ptRow :: Int -> [Int]
ptRow row = ptRowLoop row row


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