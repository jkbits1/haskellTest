module Pt2 where

import Pt

-- fac :: Int -> Int
-- fac 0 = 1
-- fac n = fac (n-1) * n

-- pt2Out n = unlines $ (map show . ptRow) $ n


pt2Out :: Int -> String
pt2Out n = ptRowAsString $ ptRow n
-- pt2Out n = unwords $ (map show . ptRow) $ n

pt2OutLines :: Int -> [String]
pt2OutLines n = (map show . ptRow) $ n

-- ShowList ShowS

-- pt2OutRows n = unwords $ (map show . ptRows) $ n
pt2OutRows :: Int -> [String]
pt2OutRows n = map ptRowAsString $ ptRows n

ptRowAsString :: [Int] -> String
ptRowAsString xs = unwords $ map show xs

pt2OutRowsString :: Int -> String
pt2OutRowsString n = unlines $ pt2OutRows n

mainX = do
    input <- readLn :: IO Int
    putStrLn $ pt2OutRowsString input

-- main = do
    -- input <- readLn :: IO Int
    -- input <- getLine
    -- putStrLn $ (map show . pt2) [1]
    -- putStrLn (read :: String -> Int)  $ input
    -- putStrLn . pt2 . (read :: String -> Int)  $ input


-- https://www.hackerrank.com/challenges/fp-filter-array
    