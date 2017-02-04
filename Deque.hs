-- working from pdf

module Deque (Deque(..)) where 
  class Deque q where
    empty   :: q a
    isEmpty :: q a -> Bool

    cons    :: a -> q a -> q a
    hdq     :: q a -> a
    tlq     :: q a -> q a

    snoc    :: q a -> a -> q a
    last    :: q a -> a
    init    :: q a -> q a
    
