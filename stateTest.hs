import Control.Monad (liftM, liftM2, liftM3, ap)
--import Control.Monad.Trans.State (state)
import System.Random

import Data.Char

generator = mkStdGen 0

randInt = fst . random $ generator :: Int

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftM2 (,) (randomRIO (1, 6)) (randomRIO (1, 6))

rollDiceIOap :: IO (Int, Int)
rollDiceIOap = return (,) `ap` (randomRIO (1, 6)) `ap` (randomRIO (1, 6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n =  sequence $ replicate n (randomRIO (1,6))

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice gen = ((r1, r2), gen3)
  where
    (r1, gen2) = randomR (1, 6) gen
    (r2, gen3) = randomR (1, 6) gen2



--state :: (Monad m)
--      => (s -> (a, s))  -- ^pure state transformer
--      -> StateT s m a   -- ^equivalent state-passing computation
--state f = StateT (return . f)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap  
  
--instance (Monad m) => Monad (State s m) where
--    return a = state $ \ s -> (a, s)  

instance Monad (State s) where
--
----  return :: a -> (State s a)
  return x = State (\st -> (x, st))
  pr >>= k = State $ \st ->
    let (x, st') = runState pr st
    in runState (k x) st'
    
get = State $ \st -> (st, st)    

put newState = State $ \_ -> ((), newState)

evalState :: State s a -> s -> a
evalState pr st = fst (runState pr st)
    
rollDie :: State StdGen Int
rollDie = State $ randomR (1, 6)

-- alt version
rollDie' :: State StdGen Int
rollDie' = 
  do  generator <- get
      let (val, newGen) = randomR (1, 6) generator
      put newGen
      return val
    
        
fromOddToEven :: Int -> (String, Int)
fromOddToEven n 
  | n `mod` 2 == 0  = ("even", n + 1)
  | otherwise       = ("odd",  n + 1)

stateOddAndEven :: State Int String
stateOddAndEven = State fromOddToEven

testOddEven :: State Int String
testOddEven = do  s1 <- get
                  return "1"

testOddEvenAlt = do s1 <- stateOddAndEven
                    return s1

testOddEvenAlt2 = do  s1 <- stateOddAndEven
                      s2 <- stateOddAndEven
                      return (s1, s2)

testOddEvenAlt3 = do  stateOddAndEven >>= 
                        \s1 -> do
                          s2 <- stateOddAndEven
                          return (s1, s2)

testOddEvenAlt3a = do  stateOddAndEven >>= 
                        \s1 -> stateOddAndEven >>= 
                          \s2 -> return (s1, s2)

flipTwo   = liftM2 (,) stateOddAndEven stateOddAndEven
--flipThree = (,,) `ap` stateOddAndEven `ap` stateOddAndEven `ap` stateOddAndEven
flipThree = liftM3 (,,) stateOddAndEven stateOddAndEven stateOddAndEven


-- muitovar.com practice

flop :: a -> (a -> b) -> b
flop x f = f x

testdiv = 3.0 `flop` (5.0 `flop` (/))
testdiv2  = 3.0 `flop` (\x -> 5.0 `flop` (\y -> (x/y)))


-- curious about type of lambda fn below
test x = (\y -> (x/y))
test2 = (\x -> (\y -> (x/y)))

divdbl :: Double -> Double -> Double
divdbl x y = x `flop` (\u -> y `flop` (\v -> (u/v)))

--newtype Wrapped a = Wrap a
newtype Wrapped a = Wrap {unwrap :: a}

wrflop :: Wrapped a -> (a -> Wrapped b) -> Wrapped b
wrflop (Wrap x) f = f x

divwrp :: Wrapped Double -> Wrapped Double -> Wrapped Double
divwrp x y = x `wrflop` (\u -> y `wrflop` (\v -> Wrap (u/v)))

instance Show a => Show (Wrapped a) where
    show (Wrap a) = show "Wrapped: " ++ (show a)

--divwrp (Wrap 3.0) $ Wrap 2.0
--unwrap $ divwrp (Wrap 3.0) $ Wrap 2.0

instance Monad Wrapped where
    (>>=) (Wrap x) f = f x
    return x = Wrap x

instance Functor Wrapped where
    fmap = liftM

instance Applicative Wrapped where
    pure = return
    (<*>) = ap

divwrp2 :: Wrapped Double -> Wrapped Double -> Wrapped Double
divwrp2 x y = do    u <- x
                    v <- y
                    return (u/v)

-- alternative definition of state monad
-- NOTE:    this bind gets x from State x on the left side of equation. This seems clearer than
--          use of runState on the right side above. Both are equal.
-- instance Monad (State s) where
--         (>>=) (State x) f = State (\st -> let (v,ns) = x st in runState (f v) ns )
--         return x = State (\s -> (x,s))

chncase :: Char -> Bool -> (Char, Bool)                     -- in - value, state    out - (nv, ns)
chncase x s | s == True = ((toLower x), False)
            | otherwise = ((toUpper x), True)


-- call chncase     with v, get s -> (nv, ns)
-- call chncasest   with v, get s -> (nv, ns) within State monad

chncasest :: Char -> State Bool Char                        -- in - value           out State S V
--chncasest x s | s == True = ((toLower x), False)
chncasest x = State (\s -> chncase x s )                    --                      fn, s -> (nv, ns)
                                                            --                      fn has v, expects s
-- !! may be wrong
chncasest2 :: Char -> State Bool Char
--chncasest2 x = State (\s -> chncase x s )
chncasest2 v =
    do  nv <- chncasest v;                                  --                      x2, s -> (nv, ns)
        return nv                                           --                      State x2
-- TEST OF TYPES - this compiles
--      return 'd'

chncasest2a :: Char -> State Bool Char
--chncasest2 x = State (\s -> chncase x s )
chncasest2a v =
--  NOTE f to the right of bind is like a no-op
    (chncasest v) >>= (\nv -> return nv)                    --                      x2, s -> (nv, ns)
-- TEST OF TYPES - this compiles
--  (chncasest x) >>= (\x2 -> return 'b')


-- runState (chncasest2a 'a') False


chncasest22 :: Char -> Char -> State Bool String
chncasest22 x y =
    do  u <- chncasest x
        v <- chncasest y
        return [u, v]

chncasest22a :: Char -> Char -> State Bool String
chncasest22a x y =
    (chncasest v) >>= (\nv ->
       (chncasest v2) >>= (\nv2 -> return [nv, nv2]))

-- runState (chncasest22a 'a' 'b') False

