module StateGame where

import Control.Monad.State 

type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame [] = do
  (_, score) <- get
  return score

playGame (x:xs) = do
  (on, score) <- get
  case x of 
    'a' | on  -> put (on,     score + 1)
    'c'       -> put (not on, score)
  playGame xs

startState :: GameState
startState = (False, 0)

-- runState (playGame "") startState
-- runState (playGame "c") startState

-- runState (playGame "ca") startState
-- runState (playGame "cac") startState

playGameMini (x:[]) = do
  (on, score) <- get
  case x of 
    'c'       -> put (not on, score)

  -- finish part
  (_, score) <- get
  return score :: State GameState GameValue
    
playGameMini' (x:[]) = do
  (on, score) <- get
  case x of 
    'c'       -> put (not on, score)

  -- finish part
  pgmBit

pgmBit =
  get >>= 
    (\(_, score) -> 
      return score :: State GameState GameValue)

playGameMini'' (x:[]) =
  get >>=
  (\(on, score) ->
    do
      case x of 
        'c'       -> 
          put (not on, score)

          -- finish part
      pgmBit
  )

playGameMini''' (x:[]) =
  get >>=
  (\(on, score) ->
    do
      case x of 
        'c'       -> 
          put (not on, score)

          -- finish part
      get >>= 
        (\(_, score) -> 
          return score :: State GameState GameValue)
  )

playGameMini'''' (x:[]) =
  get >>=
    (\(on, score) ->
      (case x of 
        'c'       -> 
          put (not on, score)) >>= 
            (\_ -> 
              get >>= 
                (\(_, score) -> 
                  return score :: State GameState GameValue)
            )
    )

-- runState (playGameMini'''' "c") startState


playGameMini''''' (x:[]) =
  get >>=
  (\(on, score) ->
      case x of 
        'c'       -> do
          put (not on, score)

          -- finish part
          pgmBit
  )

