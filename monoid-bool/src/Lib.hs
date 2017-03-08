module Lib
  ( someFunc,
    numAnd,
    NAnd,
    BAnd
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

numAnd :: (Num a, Eq a) => a -> a -> a 
numAnd x y = 
  case x of
    0 -> 0
    otherwise -> 
      case y of 
        0 -> 0
        otherwise -> 1 

newtype NAnd a = NAnd { unNAnd :: a } deriving (Show)

instance (Num a, Eq a) => Monoid (NAnd a) where
  mempty = NAnd 0
  NAnd x `mappend` NAnd y = NAnd (numAnd x y)

-- NAnd 1 `mappend` NAnd 1
-- NAnd {unNAnd = 1}


-- this early version is done correctly below
-- 
-- newtype BAnd a = BAnd { unBAnd :: a }
-- instance Monoid (BAnd Bool) where
--   mempty = BAnd True
--   BAnd x `mappend` BAnd y = BAnd (x && y)

-- this was helpful for resolving syntax issues
-- http://stackoverflow.com/a/26581127

newtype BAnd = BAnd { unBAnd :: Bool } deriving Show

instance Monoid (BAnd) where
  mempty = BAnd True
  BAnd x `mappend` BAnd y = BAnd (x && y)

-- BAnd True `mappend` BAnd True
-- BAnd {unBAnd = True}

newtype BOr = BOr { unBor :: Bool } deriving Show

instance Monoid BOr where
  mempty = BOr False
  BOr x `mappend` BOr y = BOr (x || y)

-- BOr False `mappend` BOr True
-- BOr {unBor = True}

