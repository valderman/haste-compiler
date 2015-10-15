{-# LANGUAGE OverloadedStrings #-}
-- | Basic config types for Haste's pretty printer.
module Haste.AST.PP.Opts where
import Data.ByteString.Builder
import Data.Default

-- | Pretty-printing options
data PPOpts = PPOpts {
    nameComments       :: Bool,    -- ^ Emit comments for externals?
    externalAnnotation :: Bool,    -- ^ Emit comments for names?
    useIndentation     :: Bool,    -- ^ Should we indent at all?
    indentStr          :: Builder, -- ^ Indentation step.
    useNewlines        :: Bool,    -- ^ Use line breaks?
    useSpaces          :: Bool,    -- ^ Use spaces other than where necessary?
    preserveNames      :: Bool     -- ^ Use STG names?
  }

instance Default PPOpts where
  def = PPOpts {
      nameComments        = False,
      externalAnnotation  = False,
      useIndentation      = False,
      indentStr           = "    ",
      useNewlines         = False,
      useSpaces           = False,
      preserveNames       = False
    }

-- | Print code using indentation, whitespace and newlines.
withPretty :: PPOpts -> PPOpts
withPretty opts = opts {
    useIndentation = True,
    indentStr      = "  ",
    useNewlines    = True,
    useSpaces      = True
  }

-- | Annotate non-local, non-JS symbols with qualified names.
withAnnotations :: PPOpts -> PPOpts
withAnnotations opts = opts {nameComments = True}

-- | Annotate externals with /* EXTERNAL */ comment.
withExtAnnotation :: PPOpts -> PPOpts
withExtAnnotation opts = opts {externalAnnotation = True}

-- | Preserve STG names. Currently slightly broken; don't use.
withHSNames :: PPOpts -> PPOpts
withHSNames opts = opts {preserveNames = True}
