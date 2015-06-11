{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface, CPP, MagicHash #-}
-- | JSString standard functions, to make them a more viable alternative to
--   the horribly inefficient standard Strings.
--
--   Many functions have linear time complexity due to JavaScript engines not
--   implementing slicing, etc. in constant time.
--
--   All functions are supported on both client and server, with the exception
--   of 'match', 'matches', 'regex' and 'replace', which are wrappers on top of
--   JavaScript's native regular expressions and thus only supported on the
--   client.
module Haste.JSString (
    -- | Building JSStrings
    empty, singleton, pack, cons, snoc, append, replicate,
    -- | Deconstructing JSStrings
    unpack, head, last, tail, drop, take, init, splitAt,
    -- | Examining JSStrings
    null, length, any, all,
    -- | Modifying JSStrings
    map, reverse, intercalate, foldl', foldr, concat, concatMap,
    -- | Regular expressions (client-side only)
    RegEx, match, matches, regex, replace
  ) where
import qualified Data.List
import Prelude hiding (foldr, concat, concatMap, reverse, map, all, any,
                       length, null, splitAt, init, take, drop, tail, head,
                       last, replicate)
import Data.String
import Haste.Prim
import Haste.Foreign

#ifdef __HASTE__
import GHC.Prim
import System.IO.Unsafe

{-# INLINE d2c #-}
d2c :: Double -> Char
d2c d = unsafeCoerce# d

foreign import ccall _jss_singleton :: Char -> JSString
foreign import ccall _jss_cons :: Char -> JSString -> JSString
foreign import ccall _jss_snoc :: JSString -> Char -> JSString
foreign import ccall _jss_append :: JSString -> JSString -> JSString
foreign import ccall _jss_len :: JSString -> Int
foreign import ccall _jss_index :: JSString -> Int -> Double
foreign import ccall _jss_substr :: JSString -> Int -> JSString
foreign import ccall _jss_take :: Int -> JSString -> JSString
foreign import ccall _jss_rev :: JSString -> JSString
foreign import ccall _jss_re_match :: JSString -> RegEx -> Bool
foreign import ccall _jss_re_compile :: JSString -> JSString -> RegEx
foreign import ccall _jss_re_replace :: JSString -> RegEx -> JSString -> JSString
foreign import ccall _jss_re_find :: RegEx -> JSString -> Ptr [JSString]

{-# INLINE _jss_map #-}
_jss_map :: (Char -> Char) -> JSString -> JSString
_jss_map f = _jss_cmap (_jss_singleton . f)

{-# INLINE _jss_cmap #-}
_jss_cmap :: (Char -> JSString) -> JSString -> JSString
_jss_cmap f s = unsafePerformIO $ cmap_js (return . f) s

cmap_js :: (Char -> IO JSString) -> JSString -> IO JSString
cmap_js = ffi "(function(f,s){\
var s2 = '';\
for(var i in s) {\
   s2 += f(s.charCodeAt(i));\
}\
return s2;})"

{-# INLINE _jss_foldl #-}
_jss_foldl :: (ToAny a, FromAny a) => (a -> Char -> a) -> a -> JSString -> a
_jss_foldl f x s = fromOpaque . unsafePerformIO $ do
  foldl_js (\a c ->  toOpaque $ f (fromOpaque a) c) (toOpaque x) s

foldl_js :: (Opaque a -> Char -> Opaque a)
         -> Opaque a
         -> JSString
         -> IO (Opaque a)
foldl_js = ffi "(function(f,x,s){\
for(var i in s) {\
  x = f(x,s.charCodeAt(i));\
}\
return x;})"

{-# INLINE _jss_foldr #-}
_jss_foldr :: (ToAny a, FromAny a) => (Char -> a -> a) -> a -> JSString -> a
_jss_foldr f x s = fromOpaque . unsafePerformIO $ do
  foldr_js (\c -> toOpaque . f c . fromOpaque) (toOpaque x) s

foldr_js :: (Char -> Opaque a -> Opaque a)
         -> Opaque a
         -> JSString
         -> IO (Opaque a)
foldr_js = ffi "(function(f,x,s){\
for(var i = s.length-1; i >= 0; --i) {\
  x = f(s.charCodeAt(i),x);\
}\
return x;})"

#else

{-# INLINE d2c #-}
d2c :: Char -> Char
d2c = id

_jss_singleton :: Char -> JSString
_jss_singleton c = toJSStr [c]

_jss_cons :: Char -> JSString -> JSString
_jss_cons c s = toJSStr (c : fromJSStr s)

_jss_snoc :: JSString -> Char -> JSString
_jss_snoc s c = toJSStr (fromJSStr s ++ [c])

_jss_append :: JSString -> JSString -> JSString
_jss_append a b = catJSStr "" [a, b]

_jss_len :: JSString -> Int
_jss_len s = Data.List.length $ fromJSStr s

_jss_index :: JSString -> Int -> Char
_jss_index s n = fromJSStr s !! n

_jss_substr :: JSString -> Int -> JSString
_jss_substr s n = toJSStr $ Data.List.drop n $ fromJSStr s

_jss_take :: Int -> JSString -> JSString
_jss_take n = toJSStr . Data.List.take n . fromJSStr

_jss_map :: (Char -> Char) -> JSString -> JSString
_jss_map f = toJSStr . Data.List.map f . fromJSStr

_jss_cmap :: (Char -> JSString) -> JSString -> JSString
_jss_cmap f =
  toJSStr . Data.List.concat . Data.List.map (fromJSStr . f) . fromJSStr

_jss_rev :: JSString -> JSString
_jss_rev = toJSStr . Data.List.reverse . fromJSStr

_jss_foldl :: (a -> Char -> a) -> a -> JSString -> a
_jss_foldl f x = Data.List.foldl' f x . fromJSStr

_jss_foldr :: (Char -> a -> a) -> a -> JSString -> a
_jss_foldr f x = Data.List.foldr f x . fromJSStr

_jss_re_compile :: JSString -> JSString -> RegEx
_jss_re_compile _ _ =
  error "Regular expressions are only supported client-side!"

_jss_re_match :: JSString -> RegEx -> Bool
_jss_re_match _ _ =
  error "Regular expressions are only supported client-side!"

_jss_re_replace :: JSString -> RegEx -> JSString -> JSString
_jss_re_replace _ _ _ =
  error "Regular expressions are only supported client-side!"

_jss_re_find :: RegEx -> JSString -> Ptr [JSString]
_jss_re_find _ _ =
  error "Regular expressions are only supported client-side!"

#endif

-- | A regular expression. May be used to match and replace JSStrings.
newtype RegEx = RegEx JSAny

instance IsString RegEx where
  fromString s = _jss_re_compile (fromString s) ""

-- | O(1) The empty JSString.
empty :: JSString
empty = ""

-- | O(1) JSString consisting of a single character.
singleton :: Char -> JSString
singleton = _jss_singleton

-- | O(n) Convert a list of Char into a JSString.
pack :: [Char] -> JSString
pack = toJSStr

-- | O(n) Convert a JSString to a list of Char.
unpack :: JSString -> [Char]
unpack = fromJSStr

infixr 5 `cons`
-- | O(n) Prepend a character to a JSString.
cons :: Char -> JSString -> JSString
cons = _jss_cons

infixl 5 `snoc`
-- | O(n) Append a character to a JSString.
snoc :: JSString -> Char -> JSString
snoc = _jss_snoc

-- | O(n) Append two JSStrings.
append :: JSString -> JSString -> JSString
append = _jss_append

-- | O(1) Extract the first element of a non-empty JSString.
head :: JSString -> Char
head s =
#ifdef __HASTE__
  case _jss_index s 0 of
    c | isNaN c   -> error "Haste.JSString.head: empty JSString"
      | otherwise -> d2c c -- Double/Int/Char share representation.
#else
  Data.List.head $ fromJSStr s
#endif

-- | O(1) Extract the last element of a non-empty JSString.
last :: JSString -> Char
last s =
  case _jss_len s of
    0 -> error "Haste.JSString.head: empty JSString"
    n -> d2c (_jss_index s (n-1))

-- | O(n) All elements but the first of a JSString. Returns an empty JSString
--   if the given JSString is empty.
tail :: JSString -> JSString
tail s = _jss_substr s 1

-- | O(n) Drop 'n' elements from the given JSString.
drop :: Int -> JSString -> JSString
drop n s = _jss_substr s (max 0 n)

-- | O(n) Take 'n' elements from the given JSString.
take :: Int -> JSString -> JSString
take n s = _jss_take n s

-- | O(n) All elements but the last of a JSString. Returns an empty JSString
--   if the given JSString is empty.
init :: JSString -> JSString
init s = _jss_take (_jss_len s-1) s

-- | O(1) Test whether a JSString is empty.
null :: JSString -> Bool
null s = _jss_len s == 0

-- | O(1) Get the length of a JSString as an Int.
length :: JSString -> Int
length s = _jss_len s

-- | O(n) Map a function over the given JSString.
map :: (Char -> Char) -> JSString -> JSString
map = _jss_map

-- | O(n) reverse a JSString.
reverse :: JSString -> JSString
reverse = _jss_rev

-- | O(n) Join a list of JSStrings, with a specified separator. Equivalent to
--   'String.join'.
intercalate :: JSString -> [JSString] -> JSString
intercalate = catJSStr

-- | O(n) Left fold over a JSString.
foldl' :: (ToAny a, FromAny a) => (a -> Char -> a) -> a -> JSString -> a
foldl' = _jss_foldl

-- | O(n) Right fold over a JSString.
foldr :: (ToAny a, FromAny a) => (Char -> a -> a) -> a -> JSString -> a
foldr = _jss_foldr

-- | O(n) Concatenate a list of JSStrings.
concat :: [JSString] -> JSString
concat = catJSStr ""

-- | O(n) Map a function over a JSString, then concatenate the results.
--   Note that this function is actually faster than 'map' in most cases.
concatMap :: (Char -> JSString) -> JSString -> JSString
concatMap = _jss_cmap

-- | O(n) Determines whether any character in the string satisfies the given
--   predicate.
any :: (Char -> Bool) -> JSString -> Bool
any p = Haste.JSString.foldl' (\a x -> a || p x) False

-- | O(n) Determines whether all characters in the string satisfy the given
--   predicate.
all :: (Char -> Bool) -> JSString -> Bool
all p = Haste.JSString.foldl' (\a x -> a && p x) False

-- | O(n) Create a JSString containing 'n' instances of a single character.
replicate :: Int -> Char -> JSString
replicate n c = Haste.JSString.pack $ Data.List.replicate n c

-- | O(n) Equivalent to (take n xs, drop n xs).
splitAt :: Int -> JSString -> (JSString, JSString)
splitAt n s = (Haste.JSString.take n s, Haste.JSString.drop n s)

-- | O(n) Determines whether the given JSString matches the given regular
--   expression or not.
matches :: JSString -> RegEx -> Bool
matches = _jss_re_match

-- | O(n) Find all strings corresponding to the given regular expression.
match :: RegEx -> JSString -> [JSString]
match re s = fromPtr $ _jss_re_find re s

-- | O(n) Compile a regular expression and an (optionally empty) list of flags
--   into a 'RegEx' which can be used to match, replace, etc. on JSStrings.
--
--   The regular expression and flags are passed verbatim to the browser's
--   RegEx constructor, meaning that the syntax is the same as when using
--   regular expressions in raw JavaScript.
regex :: JSString -- ^ Regular expression.
      -> JSString -- ^ Potential flags.
      -> RegEx
regex re flags = _jss_re_compile re flags

-- | O(n) String substitution using regular expressions.
replace :: JSString -- ^ String perform substitution on.
        -> RegEx    -- ^ Regular expression to match.
        -> JSString -- ^ Replacement string.
        -> JSString
replace = _jss_re_replace
