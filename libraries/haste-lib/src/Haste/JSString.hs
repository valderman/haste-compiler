{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface, CPP, MagicHash,
             GeneralizedNewtypeDeriving #-}
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
    -- * Building JSStrings
    empty, singleton, pack, cons, snoc, append, replicate,
    -- * Deconstructing JSStrings
    unpack, head, last, tail, drop, take, init, splitAt,
    -- * Examining JSStrings
    null, length, any, all,
    -- * Modifying JSStrings
    map, reverse, intercalate, foldl', foldr, concat, concatMap,
    -- * Regular expressions (client-side only)
    RegEx, match, matches, regex, replace
  ) where
import qualified Data.List
import Prelude hiding (foldr, concat, concatMap, reverse, map, all, any,
                       length, null, splitAt, init, take, drop, tail, head,
                       last, replicate)
import Data.String
import Haste.Prim
import Haste.Prim.Foreign

#ifdef __HASTE__
import GHC.Prim
import System.IO.Unsafe

{-# INLINE d2c #-}
d2c :: Double -> Char
d2c d = unsafeCoerce# d

_jss_singleton :: Char -> IO JSString
_jss_singleton = ffi "String.fromCharCode"

_jss_cons :: Char -> JSString -> IO JSString
_jss_cons = ffi "(function(c,s){return String.fromCharCode(c)+s;})"

_jss_snoc :: JSString -> Char -> IO JSString
_jss_snoc = ffi "(function(s,c){return s+String.fromCharCode(c);})"

_jss_append :: JSString -> JSString -> IO JSString
_jss_append = ffi "(function(a,b){return a+b;})"

_jss_len :: JSString -> IO Int
_jss_len = ffi "(function(s){return s.length;})"

_jss_index :: JSString -> Int -> IO Double
_jss_index = ffi "(function(s,i){return s.charCodeAt(i);})"

_jss_substr :: JSString -> Int -> IO JSString
_jss_substr = ffi "(function(s,x){return s.substr(x);})"

_jss_take :: Int -> JSString -> IO JSString
_jss_take = ffi "(function(n,s){return s.substr(0,n);})"

_jss_rev :: JSString -> IO JSString
_jss_rev = ffi "(function(s){return s.split('').reverse().join('');})"

_jss_re_match :: JSString -> RegEx -> IO Bool
_jss_re_match = ffi "(function(s,re){return s.search(re)>=0;})"

_jss_re_compile :: JSString -> JSString -> IO RegEx
_jss_re_compile = ffi "(function(re,fs){return new RegExp(re,fs);})"

_jss_re_replace :: JSString -> RegEx -> JSString -> IO JSString
_jss_re_replace = ffi "(function(s,re,rep){return s.replace(re,rep);})"

_jss_re_find :: RegEx -> JSString -> IO [JSString]
_jss_re_find = ffi "(function(re,s) {\
var a = s.match(re);\
return a ? a : [];})"

{-# INLINE _jss_map #-}
_jss_map :: (Char -> Char) -> JSString -> JSString
_jss_map f s = veryUnsafePerformIO $ cmap_js (_jss_singleton . f) s

{-# INLINE _jss_cmap #-}
_jss_cmap :: (Char -> JSString) -> JSString -> JSString
_jss_cmap f s = veryUnsafePerformIO $ cmap_js (return . f) s

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

_jss_singleton :: Char -> IO JSString
_jss_singleton c = return $ toJSStr [c]

_jss_cons :: Char -> JSString -> IO JSString
_jss_cons c s = return $ toJSStr (c : fromJSStr s)

_jss_snoc :: JSString -> Char -> IO JSString
_jss_snoc s c = return $ toJSStr (fromJSStr s ++ [c])

_jss_append :: JSString -> JSString -> IO JSString
_jss_append a b = return $ catJSStr "" [a, b]

_jss_len :: JSString -> IO Int
_jss_len s = return $ Data.List.length $ fromJSStr s

_jss_index :: JSString -> Int -> IO Char
_jss_index s n = return $ fromJSStr s !! n

_jss_substr :: JSString -> Int -> IO JSString
_jss_substr s n = return $ toJSStr $ Data.List.drop n $ fromJSStr s

_jss_take :: Int -> JSString -> IO JSString
_jss_take n = return . toJSStr . Data.List.take n . fromJSStr

_jss_map :: (Char -> Char) -> JSString -> JSString
_jss_map f = toJSStr . Data.List.map f . fromJSStr

_jss_cmap :: (Char -> JSString) -> JSString -> JSString
_jss_cmap f =
  toJSStr . Data.List.concat . Data.List.map (fromJSStr . f) . fromJSStr

_jss_rev :: JSString -> IO JSString
_jss_rev = return . toJSStr . Data.List.reverse . fromJSStr

_jss_foldl :: (a -> Char -> a) -> a -> JSString -> a
_jss_foldl f x = Data.List.foldl' f x . fromJSStr

_jss_foldr :: (Char -> a -> a) -> a -> JSString -> a
_jss_foldr f x = Data.List.foldr f x . fromJSStr

_jss_re_compile :: JSString -> JSString -> IO RegEx
_jss_re_compile _ _ =
  error "Regular expressions are only supported client-side!"

_jss_re_match :: JSString -> RegEx -> IO Bool
_jss_re_match _ _ =
  error "Regular expressions are only supported client-side!"

_jss_re_replace :: JSString -> RegEx -> JSString -> IO JSString
_jss_re_replace _ _ _ =
  error "Regular expressions are only supported client-side!"

_jss_re_find :: RegEx -> JSString -> IO [JSString]
_jss_re_find _ _ =
  error "Regular expressions are only supported client-side!"

#endif

-- | A regular expression. May be used to match and replace JSStrings.
newtype RegEx = RegEx JSAny
  deriving (ToAny, FromAny)

instance IsString RegEx where
  fromString s = veryUnsafePerformIO $ _jss_re_compile (fromString s) ""

-- | O(1) The empty JSString.
empty :: JSString
empty = ""

-- | O(1) JSString consisting of a single character.
singleton :: Char -> JSString
singleton = veryUnsafePerformIO . _jss_singleton

-- | O(n) Convert a list of Char into a JSString.
pack :: [Char] -> JSString
pack = toJSStr

-- | O(n) Convert a JSString to a list of Char.
unpack :: JSString -> [Char]
unpack = fromJSStr

infixr 5 `cons`
-- | O(n) Prepend a character to a JSString.
cons :: Char -> JSString -> JSString
cons c s = veryUnsafePerformIO $ _jss_cons c s

infixl 5 `snoc`
-- | O(n) Append a character to a JSString.
snoc :: JSString -> Char -> JSString
snoc s c = veryUnsafePerformIO $ _jss_snoc s c

-- | O(n) Append two JSStrings.
append :: JSString -> JSString -> JSString
append a b = veryUnsafePerformIO $ _jss_append a b

-- | O(1) Extract the first element of a non-empty JSString.
head :: JSString -> Char
head s =
#ifdef __HASTE__
  case veryUnsafePerformIO $ _jss_index s 0 of
    c | isNaN c   -> error "Haste.JSString.head: empty JSString"
      | otherwise -> d2c c -- Double/Int/Char share representation.
#else
  Data.List.head $ fromJSStr s
#endif

-- | O(1) Extract the last element of a non-empty JSString.
last :: JSString -> Char
last s =
  case veryUnsafePerformIO $ _jss_len s of
    0 -> error "Haste.JSString.head: empty JSString"
    n -> d2c (veryUnsafePerformIO $ _jss_index s (n-1))

-- | O(n) All elements but the first of a JSString. Returns an empty JSString
--   if the given JSString is empty.
tail :: JSString -> JSString
tail s = veryUnsafePerformIO $ _jss_substr s 1

-- | O(n) Drop 'n' elements from the given JSString.
drop :: Int -> JSString -> JSString
drop n s = veryUnsafePerformIO $ _jss_substr s (max 0 n)

-- | O(n) Take 'n' elements from the given JSString.
take :: Int -> JSString -> JSString
take n s = veryUnsafePerformIO $ _jss_take n s

-- | O(n) All elements but the last of a JSString. Returns an empty JSString
--   if the given JSString is empty.
init :: JSString -> JSString
init s = veryUnsafePerformIO $ _jss_take (veryUnsafePerformIO (_jss_len s)-1) s

-- | O(1) Test whether a JSString is empty.
null :: JSString -> Bool
null s = veryUnsafePerformIO (_jss_len s) == 0

-- | O(1) Get the length of a JSString as an Int.
length :: JSString -> Int
length = veryUnsafePerformIO . _jss_len

-- | O(n) Map a function over the given JSString.
map :: (Char -> Char) -> JSString -> JSString
map f s = _jss_map f s

-- | O(n) reverse a JSString.
reverse :: JSString -> JSString
reverse = veryUnsafePerformIO . _jss_rev

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
matches s re = veryUnsafePerformIO $ _jss_re_match s re

-- | O(n) Find all strings corresponding to the given regular expression.
match :: RegEx -> JSString -> [JSString]
match re s = veryUnsafePerformIO $ _jss_re_find re s

-- | O(n) Compile a regular expression and an (optionally empty) list of flags
--   into a 'RegEx' which can be used to match, replace, etc. on JSStrings.
--
--   The regular expression and flags are passed verbatim to the browser's
--   RegEx constructor, meaning that the syntax is the same as when using
--   regular expressions in raw JavaScript.
regex :: JSString -- ^ Regular expression.
      -> JSString -- ^ Potential flags.
      -> RegEx
regex re flags = veryUnsafePerformIO $ _jss_re_compile re flags

-- | O(n) String substitution using regular expressions.
replace :: JSString -- ^ String perform substitution on.
        -> RegEx    -- ^ Regular expression to match.
        -> JSString -- ^ Replacement string.
        -> JSString
replace s re rep = veryUnsafePerformIO $ _jss_re_replace s re rep
