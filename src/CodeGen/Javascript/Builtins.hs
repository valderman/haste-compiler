-- | Various functions generated as builtins
module CodeGen.Javascript.Builtins (toBuiltin) where
import GhcPlugins as P
import CodeGen.Javascript.AST as AST
import Control.Applicative

toBuiltin :: P.Var -> Maybe JSVar
toBuiltin v =
  case (modname, varname) of
    (Just "GHC.CString", "unpackCString#") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "unCStr"}
    (Just "GHC.Err", "error") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "die"}
    _ | otherwise ->
      Nothing
  where
    modname = moduleNameString . moduleName <$> nameModule_maybe (varName v)
    varname = occNameString $ nameOccName $ varName v
    foreignModName = moduleNameString $ name foreignModule