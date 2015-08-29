
lessThanItems [] = []
lessThanItems (x:xs) = [a | a <- xs, a <= x]

moreThanItems [] = []
moreThanItems (x:xs) = [a | a <- xs, a > x]

f [] = []

f(x:xs) =   f ys ++ [x] ++ f zs
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
