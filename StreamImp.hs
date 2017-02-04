module StreamImp (SImp) where
  import Stream

  -- data MergeSort a = MS Int [[a]] -- MS size segs
  -- data StreamCell a = Nil | Cons (a) (StreamCell a)
  data SImp a = Nil | Cons (a) (SImp a)
  -- data SImp a = SI [a] [a] deriving (Show)
  -- data QBatched a = BQ [Int] [Int] deriving (Show)

  -- mrg [] ys = ys
  -- mrg xs [] = xs
  -- mrg xs@(x:xs') ys@(y:ys') =
  --   if x <= y then
  --     x : mrg xs' ys
  --   else
  --     y : mrg xs ys'


  instance Stream SImp where
    -- empty = BQ [] []

    -- isEmpty (BQ f r)  = length f == 0

    -- queue (BQ [] r)   = BQ (reverse r) []
    -- queue (BQ f  r)   = BQ f r

    -- snoc (BQ f r) x   = BQ f (x:r)

    -- hdq (BQ [] _)     = head [] -- intended to cause exception
    -- hdq (BQ (x:f) _)  = x

    -- tlq (BQ [] _)     = BQ (tail []) [] -- intended to cause exception
    -- tlq (BQ (x:f) r)  = queue (BQ f r)

    -- take 0 _ = null

--     -- empty   :: q a
--     -- isEmpty :: q a -> Bool

--     -- cons    :: a -> q a -> q a
--     -- hdq     :: q a -> a
--     -- tlq     :: q a -> q a

--     -- snoc    :: q a -> a -> q a
--     -- last    :: q a -> a
--     -- init    :: q a -> q a

--     append  :: s a -> s a -> s a
    append Nil t = t
    append (Cons x s) t = Cons x (Stream.append s t)
    
    -- take    :: Stream s => Int -> s a -> s a
    -- take :: Stream s => Int -> s a -> s a
    take 0 _ = Nil
    take _ Nil = Nil
    take n (Cons x s) = Cons x $ Stream.take (n - 1) s

    -- drop    :: Int -> s a -> s a
    drop 0 s = s
    drop _ Nil = Nil
    drop n (Cons x s) = Stream.drop (n - 1) s

--     reverse :: s a -> s a
    reverse s =
      let 
        reverse' Nil r = r
        reverse' (Cons x s) r = reverse' s (Cons x r)
      in 
        reverse' s Nil


-- working from pdf

-- module Stream (Stream(..)) where 
--   class Stream s where
--     -- empty   :: q a
--     -- isEmpty :: q a -> Bool

--     -- cons    :: a -> q a -> q a
--     -- hdq     :: q a -> a
--     -- tlq     :: q a -> q a

--     -- snoc    :: q a -> a -> q a
--     -- last    :: q a -> a
--     -- init    :: q a -> q a

--     append  :: s a -> s a -> s a
    
--     take    :: Int -> s a -> s a
--     drop    :: Int -> s a -> s a
--     reverse :: s a -> s a
