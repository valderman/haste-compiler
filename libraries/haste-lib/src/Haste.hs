{-# LANGUAGE CPP #-}
module Haste (
    JSString, JSAny, URL, CB.GenericCallback,
    alert, prompt, eval, writeLog, catJSStr, fromJSStr,
    module M
  ) where
import Haste.Prim
import Haste.Callback as M hiding (jsSetCB, jsSetTimeout, GenericCallback)
import qualified Haste.Callback as CB (GenericCallback)
import Haste.Random as M
import Haste.JSType as M
import Haste.DOM as M
import Haste.Hash as M
import Control.Monad.IO.Class
import Control.Monad (liftM)

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
eval :: MonadIO m => String -> m String
eval js = liftIO $ liftM fromJSStr (jsEval (toJSStr js))

-- | Use console.log to write a message.
writeLog :: MonadIO m => String -> m ()
writeLog = liftIO . jsLog . toJSStr
