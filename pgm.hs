import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Word (Word8)

data Greymap = Greymap {
  greyWidth :: Int
, greyHeight :: Int
, greyMax :: Int
, greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                            " " ++ show m

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 -> 
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4) 
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) -> 
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (Greymap width height maxGrey bitmap, s6)


matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str 
  | prefix `L8.isPrefixOf` str
    = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = 
  case L8.readInt s of
    Nothing -> Nothing
    Just (num, rest) 
      | num <= 0 -> Nothing
      | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = 
  let count             = fromIntegral n
      both@(prefix, _)  = L.splitAt count str
  in
    if L.length prefix < count
    then Nothing
    else Just both

--  parseP5 $ L8.pack "P5  1 2 200 321"

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v


skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

validGrey :: (Int, L.ByteString) -> Maybe (Int, L.ByteString)
validGrey (maxGrey, s) =
  if maxGrey > 255 
  then Nothing
  else Just (maxGrey, s)

-- code as provided needs maxGrey check
parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
  matchHeader (L8.pack "P5") s  >>?
  \s -> skipSpace ((), s)       >>? 
  (getNat . snd)                >>?
  skipSpace                     >>?
  \(width, s) -> getNat s       >>?
  skipSpace                     >>?
  \(height, s) -> getNat s      >>?
  -- \(maxGrey, s) -> getBytes 1 s >>?
  validGrey                     >>?
  \(maxGrey, s) -> getBytes 1 s >>?
  (getBytes (width * height) . snd) >>?
  \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

data ParseState = ParseState {
  string :: L.ByteString
, offset :: Int64
} deriving (Show)

-- simpleParse :: ParseState -> (a, ParseState)
-- simpleParse =

-- betterParse :: ParseState -> Either String (a, ParseState)
-- betterParse =

newtype Parse a = Parse { 
  runParse :: ParseState -> Either String (a, ParseState)
}

identity :: a -> Parse a
identity a = Parse $ \s -> Right (a, s)

parse :: Parse a -> L.ByteString -> Either String a
parse parser state =
  case runParse parser (ParseState state 0) of
    Left err          -> Left err
    Right (result, _) -> Right result

-- parse (identity 1) $ L8.pack ""

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset state newOffset =
  state { offset = newOffset }

-- let before = ParseState (L8.pack "foo") 0
-- let after = modifyOffset before 3

bail :: String -> Parse a
bail err = Parse $ 
  \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

parseByte :: Parse Word8
parseByte = 
  getState ==>> parseSingleByte

parseSingleByte :: ParseState -> Parse Word8
parseSingleByte state = 
  case L.uncons (string state) of
    Nothing                 -> bail "no more input"
    Just (byte, remainder)  -> p' byte remainder state

p' :: Word8 -> L.ByteString -> ParseState -> Parse Word8
p' byte remainder state = putState newState ==>>> \_ -> identity byte
  where 
    newState  = state { string = remainder, offset = newOffset }
    newOffset = offset state + 1 

getState :: Parse ParseState
getState = Parse $ \s -> Right (s, s)

putState :: ParseState -> Parse ()
putState s = Parse $ \_ -> Right ((), s)

-- type signature works for parseSingleByte
(==>>) :: Parse ParseState -> (ParseState -> Parse Word8) -> Parse Word8
firstParse ==>> secondParser =
  firstParse ==> secondParser

(==>>>) :: Parse () -> (() -> Parse Word8) -> Parse Word8
firstParse ==>>> secondParser =
  firstParse ==> secondParser

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParse ==> secondParser =
  Parse parserChain 
    where
      parserChain = parserChainBuilder firstParse secondParser

parserChainBuilder :: Parse a -> (a -> Parse a1) -> 
                          ParseState -> Either String (a1, ParseState)
parserChainBuilder firstParse secondParser state = 
  case runParse firstParse state of
    Left errMessage ->
      Left errMessage
    Right (firstResult, newState) ->
      runParse (secondParser firstResult) newState

-- main = putStr $ show $ parse parseByte $ L8.pack "P5"
-- parse parseByte $ L8.pack "P5  1 2 200 321"

instance Functor Parse where
  fmap f parser = parser ==> \result ->
    identity (f result)

-- :step parse parseByte $ L8.pack "P5"
-- :set stop :list


  

