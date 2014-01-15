{-# LANGUAGE FlexibleInstances #-}
-- | Home-grown parser, just because.
module Haste.Parsing (
    Parse, runParser, char, charP, string, oneOf, possibly, atLeast,
    whitespace, word, Haste.Parsing.words, int, double, positiveDouble,
    suchThat, quotedString
  ) where
import Control.Applicative
import Control.Monad
import Data.Char

newtype Parse a = Parse {unP :: (String -> Maybe (String, a))}

runParser :: Parse a -> String -> Maybe a
runParser (Parse p) s =
  case p s of
    Just ("", x) -> Just x
    _            -> Nothing

instance Monad Parse where
  return x = Parse $ \s -> Just (s, x)
  Parse m >>= f = Parse $ \s -> do
    (s', x) <- m s
    unP (f x) s'

instance MonadPlus Parse where
  mplus (Parse p1) (Parse p2) = Parse $ \s ->
    case p1 s of
      x@(Just _) -> x
      _          -> p2 s
  mzero = Parse $ const Nothing

instance Functor Parse where
  fmap f (Parse g) = Parse $ fmap (fmap f) . g

instance Applicative Parse where
  pure  = return
  (<*>) = ap

-- | Require a specific character.
char :: Char -> Parse Char
char c = charP (== c)

-- | Parse a character that matches a given predicate.
charP :: (Char -> Bool) -> Parse Char
charP p = Parse $ \s ->
  case s of
    (c:next) | p c -> Just (next, c)
    _              -> Nothing  

-- | Require a specific string.
string :: String -> Parse String
string str = Parse $ \s ->
  let len        = length str
      (s', next) = splitAt len s
  in if s' == str
       then Just (next, str)
       else Nothing

-- | Apply the first matching parser.
oneOf :: [Parse a] -> Parse a
oneOf = msum

-- | Invoke a parser with the possibility of failure.
possibly :: Parse a -> Parse (Maybe a)
possibly p = oneOf [Just <$> p, return Nothing]

-- | Invoke a parser at least n times.
atLeast :: Int -> Parse a -> Parse [a]
atLeast 0 p = do
  x <- possibly p
  case x of
    Just x' -> do
      xs <- atLeast 0 p
      return (x':xs)
    _ ->
      return []
atLeast n p = do
  x <- p
  xs <- atLeast (n-1) p
  return (x:xs)

-- | Parse zero or more characters of whitespace.
whitespace :: Parse String
whitespace = atLeast 0 $ charP isSpace

-- | Parse a non-empty word. A word is a string of at least one non-whitespace
--   character.
word :: Parse String
word = atLeast 1 $ charP (not . isSpace)

-- | Parse several words, separated by whitespace.
words :: Parse [String]
words = atLeast 0 $ word <* whitespace

-- | Parse an Int.
int :: Parse Int
int = oneOf [read <$> atLeast 1 (charP isDigit),
             char '-' >> (0-) . read <$> atLeast 1 (charP isDigit)]

-- | Parse a floating point number.
double :: Parse Double
double = oneOf [positiveDouble,
                char '-' >> (0-) <$> positiveDouble]

-- | Parse a non-negative floating point number.
positiveDouble :: Parse Double
positiveDouble = do
  first <- atLeast 1 $ charP isDigit
  msecond <- possibly $ char '.' *> atLeast 1 (charP isDigit)
  case msecond of
    Just second -> return $ read $ first ++ "." ++ second
    _           -> return $ read first

-- | Fail on unwanted input.
suchThat :: Parse a -> (a -> Bool) -> Parse a
suchThat p f = do {x <- p ; if f x then return x else mzero}

-- | A string quoted with the given quotation mark.
quotedString :: Char -> Parse String
quotedString q = char q *> atLeast 0 (charP (/= q)) <* char q
