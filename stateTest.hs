import Control.Monad (liftM, liftM2, ap)
--import Control.Monad.Trans.State (state)
import System.Random

generator = mkStdGen 0

randInt = fst . random $ generator :: Int

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftM2 (,) (randomRIO (1, 6)) (randomRIO (1, 6))

rollDiceIOap :: IO (Int, Int)
rollDiceIOap = return (,) `ap` (randomRIO (1, 6)) `ap` (randomRIO (1, 6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n =  sequence $ replicate n (randomRIO (1,6))

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
    
        
