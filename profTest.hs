import System.Environment
import Text.Printf

main :: IO ()
main = do
        [d] <- ((map read) `fmap`) getArgs
--        printf "%f\n" (mean [1..d])
        printf "%f\n" (meanFoldl [1..d])

mean :: [Double] -> Double
mean xs = {-# SCC "mean" #-} sum xs / fromIntegral (length xs)

meanFoldl :: [Double] -> Double
meanFoldl xs = s / fromIntegral n
  where
    (n, s)      = foldl k (0, 0) xs
    k (n, s) x  = (n+1, s+x)
