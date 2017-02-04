module BankersDeque (QBatched) where
  import Deque

  -- data MergeSort a = MS Int [[a]] -- MS size segs
  data BankersD a = BD [a] [a] deriving (Show)
  -- data QBatched a = BQ [Int] [Int] deriving (Show)

  -- mrg [] ys = ys
  -- mrg xs [] = xs
  -- mrg xs@(x:xs') ys@(y:ys') =
  --   if x <= y then
  --     x : mrg xs' ys
  --   else
  --     y : mrg xs ys'


  instance Deque BankersD where
    empty = BQ [] []

    isEmpty (BQ f r)  = length f == 0

    queue (BQ [] r)   = BQ (reverse r) []
    queue (BQ f  r)   = BQ f r

    snoc (BQ f r) x   = BQ f (x:r)

    hdq (BQ [] _)     = head [] -- intended to cause exception
    hdq (BQ (x:f) _)  = x

    tlq (BQ [] _)     = BQ (tail []) [] -- intended to cause exception
    tlq (BQ (x:f) r)  = queue (BQ f r)

