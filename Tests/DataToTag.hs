-- Test case constributed by Till Theis

{-# LANGUAGE CPP #-}
module Tests.DataToTag where
import Control.Monad
import System.IO.Unsafe
import Control.Applicative (Applicative (..))

#ifdef __HASTE__
import Haste
output = alert
#else
output = putStrLn
#endif

trace :: Show a => a -> b -> b
trace msg = seq $ unsafePerformIO (output $ show msg)

runTest :: IO String
runTest = return . show $ parse "a"

-- | Parse a string into the token list [Tok1] and return True if the parser
-- (token Tok1) can verify that.
parse :: String -> Bool
parse s =
  case runParser tokenize s of
       Just ([], ts) -> case runParser (token Tok1) ts of
                             Just ([], _)   -> True
                             _ -> False
       _ -> False


-- | The Parser type is parameterized over the input stream element type @s@ and
-- the return type @a@. A parser takes the input and produces a return value
-- together with the not yet consumed input.
newtype Parser s a = Parser { runParser :: [s] -> Maybe ([s], a) }

parserError s msg = Nothing
parserAccept s a = Just (s, a)

instance Functor (Parser s) where
  fmap f m = m >>= return . f

instance Applicative (Parser s) where
  pure = return
  f <*> x = f >>= \f' -> fmap f' x

instance Monad (Parser s) where
  p >>= f = Parser $ \s ->
    case runParser p s of
      Just (s', a) -> runParser (f a) s'
      Nothing -> Nothing

  return = Parser . flip parserAccept
  fail = Parser . flip parserError


(<|>) :: Parser s a -> Parser s a -> Parser s a
p1 <|> p2 = Parser $ \s -> runParser p1 s `or` runParser p2 s
  where Nothing `or` b = b
        a `or` _ = a


many :: Parser s a -> Parser s [a]
many p = many1 p <|> return []

many1 :: Parser s a -> Parser s [a]
many1 p = liftM2 (:) p (many p)

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser go
  where go (x:xs) | p x = parserAccept xs x
        go s = parserError s "not satisfied"



data Token = Tok1 | Tok2 | Tok3 | Tok4 | Tok5 | Tok6 | Tok7 | Tok8 | Tok9 deriving (Show, Eq)

tokenize :: Parser Char [Token]
tokenize = many1 $ many1 (satisfy $ const True) >> return Tok1


token :: Token -> Parser Token Token
token t = satisfy (== t)
