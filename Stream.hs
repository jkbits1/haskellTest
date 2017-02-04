-- working from pdf

module Stream (Stream(..)) where 
  class Stream s where
    -- empty   :: q a
    -- isEmpty :: q a -> Bool

    -- cons    :: a -> q a -> q a
    -- hdq     :: q a -> a
    -- tlq     :: q a -> q a

    -- snoc    :: q a -> a -> q a
    -- last    :: q a -> a
    -- init    :: q a -> q a

    append  :: s a -> s a -> s a
    
    take    :: Int -> s a -> s a
    drop    :: Int -> s a -> s a
    reverse :: s a -> s a
