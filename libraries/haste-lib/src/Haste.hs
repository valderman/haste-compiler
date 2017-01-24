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

    -- * Location handling
    onHashChange, setHash, getHash,
    getLocationHref, setLocationHref, getLocationHostName, getLocationPort,

    -- * Timers
    Timer, Interval (..), setTimer, stopTimer,

    -- * Fast conversions for JS-native types
    JSType (..), JSNum (..), toString, fromString, convert,

    -- * Reflection
    getProgramId, getProgramJS
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

-- | Get the value of the @__haste_prog_id@ variable. Unless programmatically
--   changed, this variable contains the SHA3-256 hash of the currently
--   executing Haste program.
getProgramId :: IO JSString
getProgramId = ffi "(function(){return __haste_prog_id;})"

-- | Get the complete JavaScript source code of the currently executing Haste
--   program. On IE, this requires that the program's identifier, as returned
--   by 'getProgramId', has not been tampered with.
getProgramJS :: IO (Either URL JSString)
getProgramJS = do
  (murl, msrc) <- getCurrentScript
  return $ maybe (maybe impossible Right msrc) Left murl
  where
    impossible = error "impossible!"

-- | JS worker for 'getProgramJS'.
getCurrentScript :: IO (Maybe URL, Maybe JSString)
getCurrentScript = ffi "(function(){\
\    if(__haste_script_elem) {\
\        if(__haste_script_elem.innerHTML) {\
\            return [null, __haste_script_elem.innerHTML];\
\        } else {\
\            return [__haste_script_elem.src, null];\
\        }\
\    } else {\
\        var es = document.getElementsByTagName('SCRIPT');\
\        var re = new RegExp('var __haste_prog_id = \\'\\([0-9a-f]{64}\\)\\';');\
\        for(var i in es) {\
\            if(es[i].innerHTML) {\
\                var match = es[i].innerHTML.match(re);\
\                if(match && match[1] == __haste_prog_id) {\
\                    return [null, es[i].innerHTML];\
\                }\
\            } else if(es[i].src) {\
\                var xhr = new XMLHttpRequest();\
\                xhr.open('GET', es[i].src, false);\
\                xhr.send();\
\                var match = xhr.responseText.match(re);\
\                if(match && match[1] == __haste_prog_id) {\
\                    return [es[i].src, null];\
\                }\
\            }\
\        }\
\    }\
\    throw 'source of current program not found';\
\})"

-- | Get the current complete location URL.
getLocationHref :: MonadIO m => m URL
getLocationHref = liftIO getLocationHref'

getLocationHref' :: IO URL
getLocationHref' = ffi "(function(){return location.href;})"

-- | Set the location URL.
setLocationHref :: MonadIO m => URL -> m ()
setLocationHref href = liftIO (setLocationHref' href)

setLocationHref' :: URL -> IO ()
setLocationHref' = ffi "(function(href){location.href = href;})"

-- | Get the current location host name.
getLocationHostName :: MonadIO m => m JSString
getLocationHostName = liftIO getLocationHostName

getLocationHostName' :: IO JSString
getLocationHostName' = ffi "(function(){return location.hostname;})"

-- | Get the current location port.
getLocationPort :: MonadIO m => m Int
getLocationPort = liftIO getLocationPort

getLocationPort' :: IO Int
getLocationPort' = ffi "(function(){return location.port;})"
