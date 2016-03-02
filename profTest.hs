import Data.List

import System.Environment
import Text.Printf

--ghc -O2 --make ./profTest.hs -prof -auto-all -caf-all -fforce-recomp -rtsopts
--Measure-Command {.\profTest.exe 1 +RTS -hc -i0 -p}
--hp2ps -e8in -c .\profTest.hp  -- this creates .ps file
-- .\profTest.exe 1 +RTS -sstderr -- outputs to console some more info, too
main :: IO ()
main = do
        [d] <- ((map read) `fmap`) getArgs
--        printf "%f\n" (mean [1..d])
--        printf "%f\n" (meanFoldl [1..d])
--        printf "%f\n" (meanFoldl' [1..d])
        printf "%f\n" (meanNF [1..d])

mean :: [Double] -> Double
mean xs = {-# SCC "mean" #-} sum xs / fromIntegral (length xs)

-- worse than mean
meanFoldl :: [Double] -> Double
meanFoldl xs = s / fromIntegral n
  where
    (n, s)      = foldl k (0, 0) xs
    k (n, s) x  = (n+1, s+x)

-- to WHNF only
meanFoldl' :: [Double] -> Double
meanFoldl' xs = s / fromIntegral n
  where
    (n, s)      = foldl' k (0, 0) xs
    k (n, s) x  = (n+1, s+x)

-- best in time and space
meanNF :: [Double] -> Double
meanNF xs = s / fromIntegral n
  where
     (n, s)     = foldl' k (0,0) xs
     k (n, s) x = n `seq` s `seq` (n + 1, s+x)
