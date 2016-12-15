module Perm where

-- import Fac
-- import System.Console.Haskeline

strPermPart :: String -> String
strPermPart [] = []
strPermPart s = (head $ tail s) : head s : []

strPerm [] = []
strPerm s = (strPermPart $ take 2 s) ++ (strPerm $ drop 2 s)

getLines :: Int -> IO [String]
getLines n = sequence $ replicate n getLine

main1 = 
  do
    lineCount <- readLn :: IO Int
    lines <- getLines lineCount
    let permLines = map strPerm lines
    putStrLn $ unlines permLines
--    sequence_ $ map putStrLn permLines
--    return ()

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
    
mainX2 :: Int -> IO ()
mainX2 count =
  do
    putStrLn "Continue?"
    response <- getYnMax count
    let yn = fst(response)
    let newCount = snd(response)
    if newCount < 3 && yn == True
      then
        mainX2 $ newCount
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


getYnMax :: Int -> IO (Bool, Int)
getYnMax n = 
  do 
    let nextCount = n+1
    answer <- getLine
    if answer == "y"
      then
        return (True, nextCount)
      else
        return (False, nextCount)



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