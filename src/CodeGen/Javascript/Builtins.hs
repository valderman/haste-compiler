-- | Various functions generated as builtins
module CodeGen.Javascript.Builtins (toBuiltin) where
import GhcPlugins as P
import CodeGen.Javascript.AST as AST
import Control.Applicative

toBuiltin :: P.Var -> Maybe JSVar
toBuiltin v =
  case (modname, varname) of
    (Just "GHC.Prim", "coercionToken#") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "coercionToken"}    
    (Just "GHC.Prim", "realWorld#") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "realWorld"}
    (Just "GHC.Err", "error") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "err"}
    
    -- Everything to do with unpacking had better be built in for compactness,
    -- efficiency and space reasons.
    (Just "GHC.CString", "unpackCString#") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "unCStr"}
    (Just "GHC.CString", "unpackCStringUtf8#") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "unCStr"}
    (Just "GHC.CString", "unpackAppendCString#") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "unAppCStr"}
    (Just "GHC.CString", "unpackFoldrCString#") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "unFoldrCStr"}
    
    -- Primitive needs of the Haste standard library
    (Just "Haste.Prim", "toJSStr") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "toJSStr"}
    (Just "Haste.Prim", "fromJSStr") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "fromJSStr"}
    (Just "Haste.Prim", "jsRound") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "Math.round"}
    (Just "Haste.Prim", "jsCeiling") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "Math.ceil"}
    (Just "Haste.Prim", "jsFloor") ->
      Just $ JSVar {jsmod  = foreignModName,
                    jsname = Foreign "Math.floor"}
    _ | otherwise ->
      Nothing
  where
    modname = moduleNameString . moduleName <$> nameModule_maybe (varName v)
    varname = occNameString $ nameOccName $ varName v
    foreignModName = moduleNameString $ name foreignModule
