{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, OverloadedStrings #-}
-- | Haste's companion to the Prelude.
--
--   Note that this module should *not* be imported together with
--   "Haste.App", which provides the same functionality but slightly modified
--   for automatic program slicing.
module Haste (
    -- * Basic utility functions
    JSString, JSAny, URL,
    alert, prompt, eval, writeLog, catJSStr, fromJSStr,

    -- * URL hash handling
    onHashChange, onHashChange', setHash, getHash, setHash', getHash',

    -- * Timers
    Timer, Interval (..), setTimer, stopTimer,

    -- * Fast conversions for JS-native types
    JSType (..), JSNum (..), toString, fromString, convert
  ) where
import Haste.Prim
import Haste.Timer
import Haste.Prim.JSType
import Haste.Hash
import Haste.Foreign
import Control.Monad.IO.Class

jsAlert :: JSString -> IO ()
jsAlert = ffi "alert"

jsLog :: JSString -> IO ()
jsLog = ffi "(function(x){console.log(x);})"

jsPrompt :: JSString -> IO JSString
jsPrompt = ffi "(function(s){var x = prompt(s);\
\return (x === null) ? '' : x.toString();})"

jsEval :: JSString -> IO JSString
jsEval = ffi "(function(s){var x = eval(s);\
\return (typeof x === 'undefined') ? 'undefined' : x.toString();})"

-- | JavaScript @alert()@ function.
alert :: MonadIO m => JSString -> m ()
alert = liftIO . jsAlert

-- | JavaScript @prompt()@ function.
prompt :: MonadIO m => JSString -> m JSString
prompt = liftIO . jsPrompt

-- | JavaScript @eval()@ function.
eval :: MonadIO m => JSString -> m JSString
eval = liftIO . jsEval

-- | JavaScript @console.log()@.
writeLog :: MonadIO m => JSString -> m ()
writeLog = liftIO . jsLog
