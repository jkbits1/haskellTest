import System.Random
import Control.Monad.State

-- also worth a look
-- https://wiki.haskell.org/State_Monad

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

twoNotRandoms :: RandomGen g => g -> (Int, Int)
twoNotRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = 
  let 
    (a, gen') = random gen
    (b, gen'') = random gen'
  in 
    ((a, b), gen'')

-- fmap twoNotRandoms getStdGen
-- fmap twoGoodRandoms getStdGen


randomBit :: (RandomGen g, Random a) => g -> (a, g)
randomBit gen = random gen

tst  :: Num a => (a, a) -> a
tst  (a, b) = a + b
tst' :: Num a => (a, a) -> a
tst' (a, b) = (\a' b' -> a' + b') a b

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
    let 
      (val, gen') = random gen
    in
      put gen' >>
      return val

-- desugar let/where section
-- from http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html
gr2 :: Random a => RandomState a
gr2 =
  get >>= \gen ->
    (\(val, gen') -> 
      put gen' >>
      return val) $ random gen

gr2a :: Random a => RandomState a
gr2a =
  get >>= \gen ->
    (\(val, gen') -> grb (val, gen')) $ random gen

-- NOTE: doesn't compile without type signature
gR2 :: Random a => RandomState a
gR2 = 
  get >>= \gen -> 
    grb (random gen)



-- NOTE grb & grb' are the same
grb' :: MonadState s m => (b, s) -> m b
grb' (val, gen') = 
  put gen' >>
    return val

grb :: MonadState s m => (b, s) -> m b
grb (val, gen') = 
  do 
    put gen'
    return val

gR2' :: Random a => RandomState a
gR2' = 
  do 
    gen <- get
    grb (random gen)

gR2'' :: Random a => RandomState a
gR2'' = 
  do 
    gen <- get
    let (val, gen') = random gen
    grb (val, gen')

gR2''' :: Random a => RandomState a
gR2''' = 
  do 
    gen <- get
    let (val, gen') = random gen
    put gen'
    return val    

-- gr' = 
--   let f gen = 
--     do
--       let 
--         (val, gen') = random gen
--       put gen' >>
--       return val
--   in 
--     get >>= f
  
-- NOTE: first version using do syntax, was correct but
--       doesn't compile without type signature  
-- work
getRandom' :: Random a => RandomState a
-- getRandom' :: Random a => State StdGen a
-- don't work
-- getRandom' :: Random a => State m a
-- getRandom' :: State StdGen a
getRandom' = 
  do 
    -- gen <- get :: MonadState s m => m s
    gen <- get :: MonadState s m => m s
    let (val, gen') = random gen :: Random a => (a, StdGen)
    put gen'
    return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

-- twoRunRandoms :: IO (Int, Int)
twoRunRandoms = do
  oldState <- getStdGen :: IO StdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState :: IO ()
  return result :: IO (Double, Double)

-- :m +System.Random