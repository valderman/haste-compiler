-- | Haste's intermediate JavaScript representation.
module Haste.AST (
    module Constr, module Op, module Optimize, module Trav, module Opts,
    PPOpts (..), pretty, runPP, prettyProg, def,
    Arity, Comment, Name (..), Var (..), LHS, Call,
    Lit, Exp, Stm, Alt, Module (..),
    foreignModule, moduleOf, pkgOf, blackHole, blackHoleVar, merge
  ) where
import Haste.AST.Syntax
import Haste.AST.Op as Op
import Haste.AST.Optimize as Optimize
import Haste.AST.Constructors as Constr
import Haste.AST.PP (pretty, runPP, prettyProg)
import Haste.AST.PP.Opts as Opts
import Haste.AST.Print as Print ()
import Haste.AST.Binary ()
import Haste.AST.Traversal as Trav
import Data.Default (def)
