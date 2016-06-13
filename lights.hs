--https://wiki.haskell.org/Type
--https://wiki.haskell.org/Constructor#Data_constructor

stateCount = 4

data Colour = Red | Amber | Green
  deriving (Show, Enum)

-- Enum supports [Red .. Green]

type ColourSet = [Colour]

data LightState = LightState {orderNum :: Int, cols :: ColourSet}
  deriving (Show)

-- lights order
-- r ra g a
stop :: ColourSet
stop  = [Red]
ready = [Red, Amber]
go    = [Green]
slow  = [Amber]

-- orderNum $ head states
-- zip states [1..]
states :: [LightState]
states =
  [
  LightState 1 stop,
  LightState 2 ready,
  LightState 3 go,
  LightState 4 slow
  ]

start = head states

-- from
-- C:\Users\Jon\Documents\GitHub\monadMuckabout\monad-IO-2.hs

--main :: IO ()
--main = do   line <- getLine
--            if null line
--                then return ()
--                else do
--                    -- putStrLn $ line
--                    putBoard $ head line
--                    main
--
---- putBoard :: IO ()
--putBoard :: Char -> IO ()
--putBoard c =
--            -- putStrLn "_|_|_" >>
--            putRow      c     >>
--            putStrLn    "_|_|_" >>
--            putStrLn    "_|_|_"