{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, OverloadedStrings #-}
-- | Haste's companion to the Prelude.
--
--   Note that this module should *not* be imported together with
--   "Haste.App", which provides the same functionality but slightly modified
--   for automatic program slicing.
module Haste (
    JSString, JSAny, URL,
    alert, prompt, eval, writeLog, catJSStr, fromJSStr,
    module Haste.Prim.JSType, module Haste.DOM.Core, module Haste.Timer,
    module Haste.Random, module Haste.Hash
  ) where
import Haste.Prim
import Haste.Timer
import Haste.Random
import Haste.Prim.JSType
import Haste.DOM.Core
import Haste.Hash
import Haste.Foreign
import Control.Monad.IO.Class

jsAlert :: String -> IO ()
jsAlert = ffi "alert"

jsLog :: String -> IO ()
jsLog = ffi "(function(x){console.log(x);})"

jsPrompt :: String -> IO String
jsPrompt = ffi "(function(s){var x = prompt(s);\
\return (typeof x === 'undefined') ? 'undefined' : x.toString();})"

jsEval :: JSString -> IO JSString
jsEval = ffi "(function(s){var x = eval(s);\
\return (typeof x === 'undefined') ? 'undefined' : x.toString();})"

-- | Javascript alert() function.
alert :: MonadIO m => String -> m ()
alert = liftIO . jsAlert

-- | Javascript prompt() function.
prompt :: MonadIO m => String -> m String
prompt = liftIO . jsPrompt

-- | Javascript eval() function.
eval :: MonadIO m => JSString -> m JSString
eval = liftIO . jsEval

-- | Use console.log to write a message.
writeLog :: MonadIO m => String -> m ()
writeLog = liftIO . jsLog
