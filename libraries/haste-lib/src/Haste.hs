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

    -- * Random number generation (deprecated; use the @random@ package instead)
    Random (..), Seed, next, mkSeed, newSeed,

    -- * Timers
    Timer, Interval (..), setTimer, stopTimer,

    -- * Fast conversions for JS-native types
    JSType (..), JSNum (..), toString, fromString, convert
  ) where
import Haste.Prim
import Haste.Timer
import Haste.Random
import Haste.Prim.JSType
import Haste.Hash
import Haste.Foreign
import Control.Monad.IO.Class

jsAlert :: String -> IO ()
jsAlert = ffi "alert"

jsLog :: String -> IO ()
jsLog = ffi "(function(x){console.log(x);})"

jsPrompt :: String -> IO String
jsPrompt = ffi "(function(s){var x = prompt(s);\
\return (x === null) ? '' : x.toString();})"

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
