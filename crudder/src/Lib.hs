module Lib
    ( someFunc
    , addBlanks
    , ls
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- addBlanks :: [a] -> [a]
addBlanks = 
    concat . 
    map (\(x, y) ->
        case x `mod` 2 of
            0 -> [y]
            _ -> [0, y] 
    ) .
    zip [1..] 

ls = [1, 2, 5, 6]

-- addBlanks ls
-- [0,1,2,0,5,6]
