module Logger (
  Logger
, Log
, runLogger
, record
) where

import Control.Monad (liftM, liftM2, ap)

type Log = [String]

runLogger :: Logger a -> (a, Log)

record :: String -> Logger ()

globToRegex :: String -> Logger String
globToRegex cs =
  globToRegex' cs >>= \ds ->
    return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) =
  record "any" >>
  globToRegex' cs >>= \ds ->
  return ('.':ds)
globToRegex' ('*':cs) = do
  record "k star"
  ds <- globToRegex' cs
  return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) =
  record "char class, neg" >>
  charClass cs >>= \ds ->
  return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
  record "char class" >>
  charClass cs >>= \ds ->
  return ("[" ++ c : ds)
globToRegex' ('[':_) =
  fail "unterm char class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

charClass_verbose (']': cs) =
  globToRegex' cs >>= \ds ->
  return (']':ds)
charClass_verbose (c:cs) =
  charClass_verbose cs >>= \ds ->
  return (c:ds)

charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs


escape :: Char -> Logger String
escape c
  | c `elem` regexChars = record "escape" >> return ['\\', c]
  | otherwise           = return [c]
  where regexChars = "\\+()^$.{}]|"

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger = execLogger

record s = Logger ((), [s])

instance Monad Logger where
  return a = Logger (a, [])

instance Functor Logger where
  fmap = liftM

instance Applicative Logger where
  pure = return
  (<*>) = ap



