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

--     append  :: s a -> s a -> s a
    append Nil t = t
    append (Cons x s) t = Cons x (Stream.append s t)
    
    -- take    :: Stream s => Int -> s a -> s a
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



