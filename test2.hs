import Data.List
-- import Data.List.Unique

sortUniq :: Ord a => [a] -> [a]
sortUniq = nub . sort

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . group . sort

unique :: Ord a => [a] -> [a]
unique = concat . filterByLength (==1)

lessThanItems [] = []
lessThanItems (x:xs) = [a | a <- xs, a <= x]

moreThanItems [] = []
moreThanItems (x:xs) = [a | a <- xs, a > x]

qsort [] = []
qsort(x:xs) =  qsort ys ++ [x] ++ qsort zs
            where
                ys = [a | a <- xs, a <= x]
                zs = [b | b <- xs, b > x]

singleOrder [] = []
singleOrder (x:xs) = 
    lessThanItems (x : xs) 
    ++ [x] 
    ++ moreThanItems (x:xs)

build [] = []
build (x:xs) = x:xs

build2 [] = []
build2 (x:xs) = [head (x:xs)]

sO2 [] = []
sO2 (x:xs) = lessThanItems (x:xs)

mult :: Num a => a -> a -> a
mult a b = a * b

mult4 :: Num a => a -> a
mult4 = mult 4

-- isOdd a = mod a 2 == 0
isOdd a = mod a 2

mapFirst :: [[b]] -> [b] 
mapFirst = map head

uniqueMapFirst :: Eq a => [[a]] -> [a]
uniqueMapFirst xs = nub (map head xs)

uniqueMapFirst2 :: Eq a => [[a]] -> [a]
uniqueMapFirst2 = nub . map head

modFlip :: Integral a => a -> a -> a
modFlip = flip mod

isOdd2 :: Integer -> Integer
isOdd2 = modFlip 2

num2Bool :: Integer -> Bool
num2Bool 0 = False
num2Bool 1 = True

isOdd2a :: Integer -> Bool
isOdd2a a = num2Bool (modFlip 2 a)

isOdd2aa = num2Bool . modFlip 2

isOdd2b :: Integer -> Bool
isOdd2b a = num2Bool (isOdd2 a)

isOdd2ba :: Integer -> Bool
isOdd2ba = num2Bool . isOdd2

subjects = [("Combin L", "schonfinkel curry"), ("Curry-How corr", "correspondence")]

subjectTitle :: (t, t1) -> t
subjectTitle (title, _) = title

subjectDesc :: (t, t1) -> t1
subjectDesc  (_, desc)  = desc

filterSync :: Eq a => a -> (a, b) -> Bool
filterSync title subj = title == subjectTitle subj
