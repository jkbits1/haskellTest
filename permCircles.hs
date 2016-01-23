
import Data.List

inner =     [1,2,3]
second =    [4,5,6]
three =     [7,8,9]

wheelPerms :: [Int] -> [[Int]]
wheelPerms xs = permutations xs

secPerms :: [[Int]]
secPerms = wheelPerms second
-- [ [4,5,6], [5,4,6] ... ]

thrPerms :: [[Int]]
thrPerms = wheelPerms three

attachLists :: [Int] -> [Int] -> [[Int]]
attachLists inner second = inner : second : []
-- params - [1,2,3] [4,5,6]
-- result - [ [1,2,3], [4,5,6] ]

attachInnerList :: [Int] -> [[Int]]
attachInnerList = attachLists inner
-- param - [4,5,6]
-- result - [ [1,2,3], [4,5,6] ]

createTwoListPerms :: [[[Int]]]
createTwoListPerms = map attachInnerList secPerms
-- result - [ [[1,2,3], [4,5,6]],
--            [[1,2,3], [5,6,4]], ...]

appendTwoListPerms :: [Int] -> [[[Int]]]
appendTwoListPerms thr = map (\xs ->  xs ++ [thr]) createTwoListPerms
-- param = [7,8,9]
-- xs = [[Int]]
--[ [[1,2,3],[4,5,6],[7,8,9]], [[1,2,3],[5,4,6],[7,8,9]] ...]

-- !! meets answer level
createThreeListPerms :: [[[[Int]]]]
createThreeListPerms = map appendTwoListPerms thrPerms
-- result -
--[
--[ [[1,2,3],[4,5,6],[7,8,9]],  [[1,2,3],[5,4,6],[7,8,9]] ... ],
--[ [[1,2,3],[4,5,6],[8,7,9]],  [[1,2,3],[5,4,6],[8,7,9]]  ...]
-- ...
--]

--twoRings :: [Int] -> [Int] -> [[Int], [Int]]
--twoRings inner second =


inStr = "abc"
secStr = "def"
thrStr = "ghi"

secStrPerms = permutations secStr
--["def", "edf" ...]

thrStrPerms = permutations thrStr
--["ghi", "hig" ...]

thrStrPermsList = map (\s -> [s]) thrStrPerms

attachStrs :: String -> String -> [String]
attachStrs s1 s2 = s1 : s2 : []

attachInStr :: String -> [String]
attachInStr = attachStrs inStr

createTwoStrPerms :: [[String]]
createTwoStrPerms = map attachInStr secStrPerms
-- [ ["abc", "def"], ["abc", "edf"], ...]

attachListStrs :: [String] -> [String] -> [[String]]
--attachListStrs ss1 ss2 = ss1 : ss2 : []
attachListStrs ss1 ss2 = [ss1 ++ ss2]

attachTwoStrs :: [String] -> [[String]]
attachTwoStrs = attachListStrs $ head createTwoStrPerms

createThreeStrPerms :: [[[String]]]
createThreeStrPerms = map attachTwoStrs thrStrPermsList

appendStrPerms = map (\xs ->  xs ++ ["ghi"]) createTwoStrPerms
-- [ ["abc", "def", "ghi"], ["abc", "edf", "ghi"], ...]

