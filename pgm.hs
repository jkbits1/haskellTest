import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace, chr, isDigit)
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
parse parser rawState =
  case runParse parser (ParseState rawState 0) of
    Left err          -> Left err
    Right (result, _) -> Right result

-- parse (identity 1) $ L8.pack ""

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset parseState newOffset = parseState { offset = newOffset }

-- let before = ParseState (L8.pack "foo") 0
-- let after = modifyOffset before 3

bail :: String -> Parse a
bail err = Parse $ 
  \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

parseByte :: Parse Word8
parseByte = 
  getState ==>> parseSingleByte

parseSingleByte :: ParseState -> Parse Word8
parseSingleByte parseState = 
  case L.uncons (string parseState) of
    Nothing                 -> bail "no more input"
    Just (byte, remainder)  -> p' byte remainder parseState

p' :: Word8 -> L.ByteString -> ParseState -> Parse Word8
p' byte remainder parseState = putState newState ==>>> \_ -> identity byte
  where 
    newState  = parseState { string = remainder, offset = newOffset }
    newOffset = offset parseState + 1 

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
parserChainBuilder firstParse secondParser parseState = 
  case runParse firstParse parseState of
    Left errMessage ->
      Left errMessage
    Right (firstResult, newParseState) ->
      runParse (secondParser firstResult) newParseState

-- main = putStr $ show $ parse parseByte $ L8.pack "P5"
-- parse parseByte $ L8.pack "P5  1 2 200 321"

instance Functor Parse where
  fmap f parser = parser ==> \result ->
    identity (f result)

-- :step parse parseByte $ L8.pack "P5"
-- :set stop :list

-- parse parseByte L.empty
-- parse (fmap id parseByte) L.empty

--  :t runParse getState $ ParseState {string = L8.pack "", offset = 0}
--  :t parseSingleByte ParseState {string = L8.pack "", offset = 0}


-- (runParse $ (parseSingleByte ParseState {string = L8.pack "", offset = 0})) ParseState {string = L8.pack "", offset = 0}

-- (runParse $ (parseSingleByte ParseState {string = L8.pack "P", offset = 0})) ParseState {string = L8.pack "P", offset = 0}

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

-- getState :: Parse ParseState
peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

-- runParse peekByte ParseState {string = L8.empty, offset = 0}
-- Right (Nothing,ParseState {string = "", offset = 0})

-- final attempt, rwh version - fmap w2c <$> peekByte
-- peekChar :: Parse (Maybe Char)
-- peekChar = w2c <$> peekByte
peekChar :: Parse (Maybe Char)
peekChar  = (\a -> w2c <$> a) <$> peekByte 
peekChar1 = (peekCharBit <$> peekByte)  where peekCharBit a = w2c <$> a    
peekChar2 = (fmap peekCharBit peekByte) where peekCharBit a = fmap w2c a    
peekChar3 = (peekCharBit <$> peekByte)  where peekCharBit = \a -> w2c <$> a    

-- runParse peekChar ParseState {string = L8.pack "P", offset = 0}
-- Right (Just 'P',ParseState {string = "P", offset = 0})

peekCharPart :: Maybe Word8 -> Maybe Char
peekCharPart a = w2c <$> a 

-- runParse (peekCharPart <$> peekByte) ParseState {string = L8.pack "P", offset = 0}
-- Right (Just 'P',ParseState {string = "P", offset = 0})

peekCharPart' :: Parse (Maybe Word8)
peekCharPart' = id <$> peekByte

peekCharPart'' :: Maybe Word8 -> Word8
peekCharPart'' a = 
  case a of 
    Nothing -> 0
    Just w -> w

-- :t peekCharPart'' <$> peekByte :: Parse Word8
testPeekCharPart'' = 
  runParse (peekCharPart'' <$> peekByte) ParseState {string = L8.empty, offset = 0}

-- :t w2c <$> peekCharPart'' <$> peekByte :: Parse Char

peekCharPart''' :: Maybe Word8 -> Char
peekCharPart''' a = 
  case a of 
    Nothing -> w2c 0
    Just w -> w2c w

-- :t peekCharPart''' <$> peekByte :: Parse Char

peekCharPart'''' :: Maybe Word8 -> Maybe Char
peekCharPart'''' a = 
  case a of 
    Nothing -> Just $ w2c 0
    Just w -> Just $ w2c w

-- :t peekCharPart'''' <$> peekByte :: Parse (Maybe Char)

testPeekCharPart'''' = 
  runParse (peekCharPart'''' <$> peekByte) ParseState {string = L8.empty, offset = 0}

parseWhileVerbose :: (Word8 -> Bool) -> Parse [Word8]
parseWhileVerbose p =
  peekByte ==> \mc ->
    case mc of
      Nothing -> identity []
      Just c | p c ->
                  parseByte ==> \b ->
                  parseWhileVerbose p ==> \bs ->
                  identity (b:bs)
              | otherwise ->
                  identity []

-- runParse (parseWhileVerbose (\_ -> True)) ParseState {string = L8.pack "P5 1", offset = 0}
-- Right ([80],ParseState {string = "", offset = 1})

-- runParse (parseWhileVerbose (\c -> notElem (w2c c) " \n" )) ParseState {string = L8.pack "P5 1", offset = 0}
-- Right ([80,53],ParseState {string = " 1", offset = 2})

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p =
  (fmap p <$> peekByte) ==> \mp ->
    if mp == Just True
    then parseByte ==> \b ->
      (b:) <$> parseWhile p
    else identity []

-- very basic implementation 
notWhiteSpace = \c -> notElem c " \n\r\t"

parseRawPGM =
  parseWhileWith w2c notWhiteSpace ==> \header -> skipSpaces ==>&
  assert (header == "P5") "invalid header" ==>&
  parseNat ==>  \width    -> skipSpaces ==>&
  parseNat ==>  \height   -> skipSpaces ==>&
  parseNat ==>  \maxGrey  -> parseByte ==>&
  parseBytes (width * height) ==> 
                \bitmap   -> identity (Greymap width height maxGrey bitmap)

-- runParse parseRawPGM ParseState {string = L8.pack "P5 1 2 3 123456", offset = 0}

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

-- runParse (parseWhileWith w2c notSeparator) ParseState {string = L8.pack "P5 1", offset = 0}

parseNat :: Parse Int
parseNat = 
  parseWhileWith w2c isDigit ==> \digits ->
    if null digits
    then bail "no more input"
    else 
      let n = read digits
      in 
        if n < 0
        then bail "integer overflow"
        else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _     = identity ()
assert False err  = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
  getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st {offset = offset st + L.length h, string = t}
    in
      putState st' ==>&
        assert (L.length h == n') "end of input" ==>&
          identity h



