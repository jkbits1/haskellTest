module Lib
    ( someFunc
    , Tree
    ) where

import Data.Foldable
import Data.Monoid

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x left right) = foldMap f left `mappend` f x `mappend` foldMap f right

t1 = Node 1 Empty Empty :: Tree Int

t2 = Node 1 Empty Empty :: Tree Int

ts1 = Node 1 Empty Empty :: Tree (Sum Int)

ts2 = Node (Sum 2) Empty Empty :: Tree (Sum Int)

ts3 = Node (Sum 3) (Node (Sum 4) Empty Empty) Empty :: Tree (Sum Int)

ts3a = Node 3 (Node 4 Empty Empty) Empty :: Tree Int

-- foldMap id ts3
-- Sum {getSum = 7}

-- Data.Foldable.fold ts3
-- Sum {getSum = 7}

-- Data.Foldable.foldr mappend 0 ts3
-- Sum {getSum = 7}

-- Data.Foldable.foldr (+) 0 ts3
-- Sum {getSum = 7}

-- Data.Foldable.foldl' (+) 0 ts3
-- Sum {getSum = 7}

data Tree2 a = Empty2 | Node2 a (Tree2 a) (Tree2 a) deriving Show

instance Foldable Tree2 where
    foldr f z Empty2 = z
    foldr f z (Node2 x Empty2 Empty2) = f x z
    foldr f z (Node2 x l r) = foldr f (f x (foldr f z r)) l

tt1 = Node2 1 Empty2 Empty2 :: Tree2 Int

tt2 = Node2 1 Empty2 Empty2 :: Tree2 Int

tts1 = Node2 1 Empty2 Empty2 :: Tree2 Int

tts2 = Node2 2 Empty2 Empty2 :: Tree2 Int

tts3 = Node2 3 (Node2 4 Empty2 Empty2) Empty2 :: Tree2 Int

tts4 =
    Node2 1 
        (Node2 2 
            (Node2 3 
                (Empty2) 
                (Node2 4 Empty2 Empty2)
            ) 
            (Node2 5 Empty2 Empty2) 
        )
        (Node2 6 
            Empty2 
            (Node2 7 Empty2 Empty2)
        )

