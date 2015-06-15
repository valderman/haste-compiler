-- | Javascript subset as a target language for compilers.
module Data.JSTarget (
    module Constr, module Op, module Optimize, module Trav,
    PPOpts (..), pretty, runPP, prettyProg, def,
    Arity, Comment, Name (..), Var (..), LHS, Call,
    Lit, Exp, Stm, Alt, AST (..), Module (..),
    foreignModule, moduleOf, pkgOf, blackHole, blackHoleVar, merge
  ) where
import Data.JSTarget.AST
import Data.JSTarget.Op as Op
import Data.JSTarget.Optimize as Optimize
import Data.JSTarget.Constructors as Constr
import Data.JSTarget.PP (pretty, runPP, prettyProg, PPOpts (..))
import Data.JSTarget.Print as Print ()
import Data.JSTarget.Binary ()
import Data.JSTarget.Traversal as Trav
import Data.Default (def)
