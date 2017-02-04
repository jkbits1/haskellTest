-- working from pdf

module Queue (Queue(..)) where 
  class Queue q where
    empty   :: q a
    isEmpty :: q a -> Bool
    snoc    :: q a -> a -> q a
    hdq     :: q a -> a
    tlq     :: q a -> q a

    queue   :: q a -> q a
    
