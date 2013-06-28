-- | Javascript subset as a target language for compilers.
module Data.JSTarget (
    module Constr, module Print, module Op, module Optimize,
    PPOpts (..), pretty, def,
    Arity, Comment, Shared, Name, zeroName, Var, LHS, Call,
    Lit, Exp, Stm, Alt
  ) where
import Data.JSTarget.AST
import Data.JSTarget.Op as Op
import Data.JSTarget.Optimize as Optimize
import Data.JSTarget.Constructors as Constr
import Data.JSTarget.PP (pretty, PPOpts (..))
import Data.JSTarget.Print as Print ()
import Data.JSTarget.Binary
import Data.Default (def)
