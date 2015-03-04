{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances,
             GeneralizedNewtypeDeriving, CPP #-}
-- | JSTarget pretty printing machinery. The actual printing happens in 
--   Data.JSTarget.Print.
module Data.JSTarget.PP where
import Data.Default
import Data.Monoid
import Data.String
import Data.List (foldl')
import Data.Array
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import Data.JSTarget.AST (AST (..), Name (..), JumpTable, Lbl, Stm)
import Data.JSTarget.Traversal (JSTrav)
import Data.ByteString.Builder

-- | Pretty-printing options
data PPOpts = PPOpts {
    nameComments   :: Bool,    -- ^ Emit comments for names, where available?
    useIndentation :: Bool,    -- ^ Should we indent at all?
    indentStr      :: Builder, -- ^ String to use for each step of indentation
    useNewlines    :: Bool,    -- ^ Use line breaks?
    useSpaces      :: Bool     -- ^ Use spaces other than where necessary?
  }

type IndentLvl = Int

-- | Final name for symbols. This name is what actually appears in the final
--   JS dump, albeit "base 62"-encoded.
newtype FinalName = FinalName Int deriving (Ord, Eq, Enum, Show)
type NameSupply = (FinalName, M.Map Name FinalName)

emptyNS :: NameSupply
emptyNS = (FinalName 0, M.empty)

newtype PP a = PP {unPP :: PPOpts
                        -> IndentLvl
                        -> NameSupply
                        -> JumpTable
                        -> Builder
                        -> (NameSupply, Builder, a)}

instance Monad PP where
  PP m >>= f = PP $ \opts indentlvl ns js b ->
    case m opts indentlvl ns js b of
      (ns', b', x) -> unPP (f x) opts indentlvl ns' js b'
  return x = PP $ \_ _ ns _ b -> (ns, b, x)

instance Applicative PP where
  pure  = return
  (<*>) = ap

instance Functor PP where
  fmap f p = p >>= return . f

-- | Convenience operator for using the PP () IsString instance.
(.+.) :: PP () -> PP () -> PP ()
(.+.) = (>>)
infixl 1 .+.

instance Default PPOpts where
  def = PPOpts {
      nameComments   = False,
      useIndentation = False,
      indentStr      = "    ",
      useNewlines    = False,
      useSpaces      = False
    }

debugPPOpts :: PPOpts
debugPPOpts = def {
    nameComments   = True,
    useIndentation = True,
    indentStr      = "  ",
    useNewlines    = True,
    useSpaces      = True
  }

-- | Generate the final name for a variable.
finalNameFor :: Name -> PP FinalName
finalNameFor n = PP $ \_ _ ns@(nextN, m) _ b ->
  case M.lookup n m of
    Just n' -> (ns, b, n')
    _       -> ((succ nextN, M.insert n nextN m), b, nextN)

-- | Look up a shared reference.
lookupLabel :: Lbl -> PP Stm
lookupLabel lbl = PP $ \_ _ ns js b -> (ns, b, js M.! lbl)

-- | Returns the value of the given pretty-printer option.
getOpt :: (PPOpts -> a) -> PP a
getOpt f = PP $ \opts _ ns _ b -> (ns, b, f opts)

-- | Runs the given printer iff the specifiet option is True.
whenOpt :: (PPOpts -> Bool) -> PP () -> PP ()
whenOpt f p = getOpt f >>= \x -> when x p

-- | Pretty print an AST.
pretty :: (JSTrav a, Pretty a) => PPOpts -> AST a -> BS.ByteString
pretty opts (AST ast js) =
  case runPP opts js (pp ast) of
    (b, _) -> toLazyByteString b

-- | Run a pretty printer.
runPP :: PPOpts -> JumpTable -> PP a -> (Builder, a)
runPP opts js p =
  case unPP p opts 0 emptyNS js mempty of
    (_, b, x) -> (b, x)

-- | Pretty-print a program and return the final name for its entry point.
prettyProg :: Pretty a => PPOpts -> Name -> AST a -> (Builder, Builder)
prettyProg opts mainSym (AST ast js) = runPP opts js $ do
  pp ast
  buildFinalName <$> finalNameFor mainSym

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
indent (PP p) = PP $ \opts indentlvl ns b ->
  if useIndentation opts
    then p opts (indentlvl+1) ns b
    else p opts 0 ns b

class Buildable a where
  put :: a -> PP ()

instance Buildable Builder where
  put x = PP $ \_ _ ns _ b -> (ns, b <> x, ())
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
ind = PP $ \opts indentlvl ns _ b ->
  (ns, foldl' (<>) b (replicate indentlvl (indentStr opts)), ())

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
