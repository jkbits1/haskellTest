module Pt2 where

fac :: Int -> Int
fac 0 = 1
fac n = fac (n-1) * n

pt2 :: Int -> [Int]
pt2 row = [fac row]

pt2Out :: Int -> String
pt2Out n = unlines $ (map show . pt2) $ fac n

pt2OutLines :: Int -> [String]
pt2OutLines n = (map show . pt2) $ fac n

-- mainX = do
    -- input <- readLn :: IO Int
    -- putStrLn $ pt2Out input

-- main = do
    -- input <- readLn :: IO Int
    -- input <- getLine
    -- putStrLn $ (map show . pt2) [1]
    -- putStrLn (read :: String -> Int)  $ input
    -- putStrLn . pt2 . (read :: String -> Int)  $ input


-- https://www.hackerrank.com/challenges/fp-filter-array
    