{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, PatternGuards #-}
-- | Haste-specific JSON library. JSON is common enough that it's a good idea
--   to create as fast and small an implementation as possible. To that end,
--   the parser is implemented entirely in Javascript, and works with any
--   browser that supports JSON.parse; IE does this from version 8 and up, and
--   everyone else has done it since just about forever.
module Haste.JSON (JSON (..), encode, decode) where
import Haste
import Haste.Prim
import Foreign.Ptr
import Data.String

-- Remember to update jsParseJSON if this data type changes!
data JSON
  = Num  Double
  | Str  JSString
  | Bool Bool
  | Arr  [JSON]
  | Dict [(JSString, JSON)]

instance IsString JSON where
  fromString = Str . fromString

numFail :: a
numFail = error "Num JSON: not a numeric JSON node!"

-- | This instance may be a bad idea, but it's nice to be able to create JSOn
--   objects using plain numeric literals.
instance Num JSON where
  (Num a) + (Num b) = Num (a+b)
  _ + _             = numFail
  (Num a) * (Num b) = Num (a*b)
  _ * _             = numFail
  (Num a) - (Num b) = Num (a-b)
  _ - _             = numFail
  negate (Num a)    = Num (negate a)
  negate _          = numFail
  abs (Num a)       = Num (abs a)
  abs _             = numFail
  signum (Num a)    = signum (Num a)
  signum _          = numFail
  fromInteger n     = Num (fromInteger n)

foreign import ccall "jsShow" jsShowD :: Double -> JSString
foreign import ccall "jsUnquote" jsUnquote :: JSString -> JSString
foreign import ccall "jsParseJSON" jsParseJSON :: JSString -> Ptr JSON

encode :: JSON -> JSString
encode = catJSStr . enc []
  where
    comma   = ","
    openbr  = "["
    closebr = "]"
    opencu  = "{"
    closecu = "}"
    colon   = ":"
    quote   = "\""
    true    = "true"
    false   = "false"
    enc acc (Str s)      = quote : jsUnquote s : quote : acc
    enc acc (Num d)      = jsShowD d : acc
    enc acc (Bool True)  = true : acc
    enc acc (Bool False) = false : acc
    enc acc (Arr elems)
      | (x:xs) <- elems =
        openbr : enc (foldr (\s a -> comma:enc a s) (closebr:acc) xs) x
      | otherwise =
        openbr : closebr : acc
    enc acc (Dict elems)
      | ((key,val):xs) <- elems =
        let encElem (k, v) a = comma : quote : k : quote : colon : enc a v
            encAll = opencu : quote : jsUnquote key : quote : colon : encRest
            encRest  = enc (foldr encElem (closecu:acc) xs) val
        in encAll
      | otherwise =
        opencu : closecu : acc

decode :: JSString -> JSON
decode = unsafeUnPtr . jsParseJSON
