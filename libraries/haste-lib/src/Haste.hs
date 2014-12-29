{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
-- | Haste's companion to the Prelude.
--
--   Note that this module should *not* be imported together with
--   "Haste.App", which provides the same functionality but slightly modified
--   for automatic program slicing.
module Haste (
    JSString, JSAny, URL, CB.GenericCallback,
    alert, prompt, eval, writeLog, catJSStr, fromJSStr,
    module Haste.JSType, module Haste.DOM.Core, module Haste.Callback,
    module Haste.Random, module Haste.Hash
  ) where
import Haste.Prim
import Haste.Callback hiding (jsSetCB, jsSetTimeout, GenericCallback)
import qualified Haste.Callback as CB (GenericCallback)
import Haste.Random
import Haste.JSType
import Haste.DOM.Core
import Haste.Hash
import Control.Monad.IO.Class

#ifdef __HASTE__
foreign import ccall jsAlert  :: JSString -> IO ()
foreign import ccall jsLog    :: JSString -> IO ()
foreign import ccall jsPrompt :: JSString -> IO JSString
foreign import ccall jsEval   :: JSString -> IO JSString
#else
jsAlert  :: JSString -> IO ()
jsAlert = error "Tried to use jsAlert on server side!"
jsLog    :: JSString -> IO ()
jsLog = error "Tried to use jsLog on server side!"
jsPrompt :: JSString -> IO JSString
jsPrompt = error "Tried to use jsPrompt on server side!"
jsEval   :: JSString -> IO JSString
jsEval = error "Tried to use jsEval on server side!"
#endif

-- | Javascript alert() function.
alert :: MonadIO m => String -> m ()
alert = liftIO . jsAlert . toJSStr

-- | Javascript prompt() function.
prompt :: MonadIO m => String -> m String
prompt q = liftIO $ do
  a <- jsPrompt (toJSStr q)
  return (fromJSStr a)

-- | Javascript eval() function.
eval :: MonadIO m => JSString -> m JSString
eval = liftIO . jsEval

-- | Use console.log to write a message.
writeLog :: MonadIO m => String -> m ()
writeLog = liftIO . jsLog . toJSStr
