{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
-- | JSTarget pretty printing machinery. The actual printing happens in 
--   Data.JSTarget.Print.
module Data.JSTarget.PP where
import Data.Default
import Data.Monoid
import Data.String
import Data.List (foldl')
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.JSTarget.AST (AST (..), JumpTable, Lbl, Stm, freeze)

-- | Pretty-printing options
data PPOpts = PPOpts {
    nameComments   :: Bool,    -- ^ Emit comments for names, where available?
    useIndentation :: Bool,    -- ^ Should we indent at all?
    indentStr      :: Builder, -- ^ String to use for each step of indentation
    useNewlines    :: Bool     -- ^ Use line breaks?
  }

type IndentLvl = Int

newtype PP a = PP {unPP :: PPOpts
                        -> JumpTable
                        -> IndentLvl
                        -> Builder
                        -> (Builder, a)}

instance Monad PP where
  PP m >>= f = PP $ \opts js indentlvl b ->
    case m opts js indentlvl b of
      (b', x) -> unPP (f x) opts js indentlvl b'
  return x = PP $ \_ _ _ b -> (b, x)

-- | Convenience operator for using the PP () IsString instance.
(.+.) :: PP () -> PP () -> PP ()
(.+.) = (>>)
infixl 1 .+.

instance Default PPOpts where
  def = PPOpts {
      nameComments   = True,
      useIndentation = True,
      indentStr      = "  ",
      useNewlines    = True
    }

-- | Get the definition for a label.
resolve :: Lbl -> PP Stm
resolve lbl = PP $ \_ js _ b -> (b, js M.! lbl)

-- | Returns the value of the given pretty-printer option.
getOpt :: (PPOpts -> a) -> PP a
getOpt f = PP $ \opts _ _ b -> (b, f opts)

-- | Runs the given printer iff the specifiet option is True.
whenOpt :: (PPOpts -> Bool) -> PP () -> PP ()
whenOpt f p = getOpt f >>= \x -> when x p

-- | Pretty print an AST.
pretty :: Pretty a => PPOpts -> AST a -> BS.ByteString
pretty opts ast =
  case freeze ast of
    (x, js) -> toLazyByteString $ fst $ unPP (pp x) opts js 0 mempty

-- | Indent the given builder another step.
indent :: PP a -> PP a
indent (PP p) = PP $ \opts js indentlvl b ->
  if useIndentation opts
    then p opts js (indentlvl+1) b
    else p opts js 0 b

class Buildable a where
  put :: a -> PP ()

instance Buildable Builder where
  put x = PP $ \_ _ _ b -> (b <> x, ())
instance Buildable String where
  put = put . stringUtf8
instance Buildable Char where
  put = put . char7
instance Buildable Int where
  put = put . intDec
instance Buildable Double where
  put = put . doubleDec
instance Buildable Integer where
  put = put . integerDec
instance Buildable Bool where
  put x = put $ if x then string7 "true" else string7 "false"

-- | Emit indentation up to the current level.
ind :: PP ()
ind = PP $ \opts _ indentlvl b ->
  (foldl' (<>) b (replicate indentlvl (indentStr opts)), ())

-- | Indent the given builder and terminate it with a newline.
line :: PP () -> PP ()
line p = do
  ind >> p
  whenOpt useNewlines $ put '\n'

ppList :: Pretty a => Builder -> [a] -> PP ()
ppList sep (x:xs) =
  foldl' (\l r -> l >> put sep >> pp r) (pp x) xs

instance IsString Builder where
  fromString = stringUtf8

instance IsString (PP ()) where
  fromString = put . stringUtf8

-- | Pretty-printer class. Each part of the AST needs an instance of this.
class Pretty a where
  pp :: a -> PP ()
