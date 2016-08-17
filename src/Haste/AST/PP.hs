{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances,
             GeneralizedNewtypeDeriving, CPP #-}
-- | Haste AST pretty printing machinery. The actual printing happens in 
--   Haste.AST.Print.
module Haste.AST.PP where
import Data.Monoid
import Data.String
import Data.List (foldl')
import Data.Array
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSS
import Data.ByteString (ByteString)
import Haste.AST.Syntax (Name (..))
import Data.ByteString.Builder
import Haste.Config
import Haste.AST.PP.Opts

type IndentLvl = Int

-- | Final name for symbols. This name is what actually appears in the final
--   JS dump, albeit "base 62"-encoded.
newtype FinalName = FinalName Int deriving (Ord, Eq, Enum, Show)
type NameSupply = (FinalName, M.Map Name FinalName)

emptyNS :: NameSupply
emptyNS = (FinalName 0, M.empty)

newtype PP a = PP {unPP :: Config
                        -> IndentLvl
                        -> NameSupply
                        -> Builder
                        -> (NameSupply, Builder, a)}

instance Monad PP where
  PP m >>= f = PP $ \cfg indentlvl ns b ->
    case m cfg indentlvl ns b of
      (ns', b', x) -> unPP (f x) cfg indentlvl ns' b'
  return x = PP $ \_ _ ns b -> (ns, b, x)

instance Applicative PP where
  pure  = return
  (<*>) = ap

instance Functor PP where
  fmap f p = p >>= return . f

-- | Convenience operator for using the PP () IsString instance.
(.+.) :: PP () -> PP () -> PP ()
(.+.) = (>>)
infixl 1 .+.

-- | Generate the final name for a variable.
--   Up until this point, internal names may be just about anything.
--   The "final name" scheme ensures that all internal names end up with a
--   proper, unique JS name.
finalNameFor :: Name -> PP FinalName
finalNameFor n = PP $ \_ _ ns@(nextN, m) b ->
  case M.lookup n m of
    Just n' -> (ns, b, n')
    _       -> ((succ nextN, M.insert n nextN m), b, nextN)

-- | Returns the value of the given pretty-printer option.
getOpt :: (PPOpts -> a) -> PP a
getOpt f = getCfg (f . ppOpts)

-- | Runs the given printer iff the specified PP option is True.
whenOpt :: (PPOpts -> Bool) -> PP () -> PP ()
whenOpt f p = getOpt f >>= \x -> when x p

-- | Returns the value of the given pretty-printer option.
getCfg :: (Config -> a) -> PP a
getCfg f = PP $ \cfg _ ns b -> (ns, b, f cfg)

-- | Pretty print an AST.
pretty :: Pretty a => Config -> a -> BS.ByteString
pretty cfg ast =
  case runPP cfg (pp ast) of
    (b, _) -> toLazyByteString b

-- | Run a pretty printer with the given name supply.
runPPWith :: Config -> NameSupply -> PP a -> (Builder, a)
runPPWith cfg ns p =
  case unPP p cfg 0 ns mempty of
    (_, b, x) -> (b, x)

-- | Run a pretty printer with an empty name supply.
runPP :: Config -> PP a -> (Builder, a)
runPP cfg = runPPWith cfg emptyNS

-- | Takes a config, a program entry point, and a pretty-printable program.
--   Returns the pretty-printed program, the final names for all static
--   pointers in the program, and the final name of the main symbol, in that
--   order.
prettyProg :: Pretty a => Config -> Name -> [Name] -> a -> (Builder, ([Builder], Builder))
prettyProg cfg mainSym spt ast = runPP cfg $ do
  pp ast
  hsnames <- getOpt preserveNames
  main <- if hsnames
            then return $ buildStgName mainSym
            else buildFinalName <$> finalNameFor mainSym
  spt' <- mapM (fmap buildFinalName . finalNameFor) spt
  return (spt', main)

-- | JS-mangled version of an internal name.
buildStgName :: Name -> Builder
buildStgName (Name n mq) =
    byteString "$hs$" <> qual <> byteString (BSS.map mkjs n)
  where
    qual = case mq of
             Just (_, m) -> byteString (BSS.map mkjs m) <> byteString "$"
             _           -> mempty
    mkjs c
      | c >= 'a' && c <= 'z' = c
      | c >= 'A' && c <= 'Z' = c
      | c >= '0' && c <= '9' = c
      | c == '$'             = c
      | otherwise            = '_'

-- | Turn a FinalName into a Builder.
buildFinalName :: FinalName -> Builder
buildFinalName (FinalName 0) =
    fromString "_0"
buildFinalName (FinalName fn) =
    charUtf8 '_' <> go fn mempty
  where
      arrLen = 62
      chars = listArray (0,arrLen-1)
              $ "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      go 0 acc = acc
      go n acc = let (rest, ix) = n `quotRem` arrLen 
                 in go rest (charUtf8 (chars ! ix) <> acc)

-- | Indent the given builder another step.
indent :: PP a -> PP a
indent (PP p) = PP $ \cfg indentlvl ns b ->
  if useIndentation (ppOpts cfg)
    then p cfg (indentlvl+1) ns b
    else p cfg 0 ns b

class Buildable a where
  put :: a -> PP ()

instance Buildable Builder where
  put x = PP $ \_ _ ns b -> (ns, b <> x, ())
instance Buildable ByteString where
  put = put . byteString
instance Buildable String where
  put = put . stringUtf8
instance Buildable Char where
  put = put . charUtf8
instance Buildable Int where
  put = put . intDec
instance Buildable Double where
  put d =
    case round d of
      n | fromIntegral n == d -> put $ intDec n
        | otherwise           -> put $ doubleDec d
instance Buildable Integer where
  put = put . integerDec
instance Buildable Bool where
  put True  = "true"
  put False = "false"

-- | Emit indentation up to the current level.
ind :: PP ()
ind = PP $ \cfg indentlvl ns b ->
  (ns, foldl' (<>) b (replicate indentlvl (indentStr $ ppOpts cfg)), ())

-- | A space character.
sp :: PP ()
sp = whenOpt useSpaces $ put ' '

-- | A newline character.
newl :: PP ()
newl = whenOpt useNewlines $ put '\n'

-- | Indent the given builder and terminate it with a newline.
line :: PP () -> PP ()
line p = do
  ind >> p
  whenOpt useNewlines $ put '\n'

-- | Pretty print a list with the given separator.
ppList :: Pretty a => PP () -> [a] -> PP ()
ppList sep (x:xs) =
  foldl' (\l r -> l >> sep >> pp r) (pp x) xs
ppList _ _ =
  return ()

instance IsString (PP ()) where
  fromString = put . stringUtf8

-- | Pretty-printer class. Each part of the AST needs an instance of this.
class Pretty a where
  pp :: a -> PP ()
