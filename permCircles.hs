
import Data.List

inner =     [1,2,3]
second =    [4,5,6]
three =     [7,8,9]

secPerms :: [[Int]]
secPerms = permutations second

thrPerms :: [[Int]]
thrPerms = permutations three

attachLists :: [Int] -> [Int] -> [[Int]]
attachLists inner second = inner : second : []

attachInnerList :: [Int] -> [[Int]]
attachInnerList = attachLists inner

createListPerms :: [[[Int]]]
createListPerms = map attachInnerList secPerms
-- [ [[1,2,3], [4,5,6]], [[1,2,3], [5,6,4]], ...]


appendListPerms = map (\xs ->  xs ++ [[7,8,9]]) createListPerms
-- xs = [[Int]]

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

