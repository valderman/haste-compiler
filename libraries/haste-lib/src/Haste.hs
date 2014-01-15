{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
module Haste (JSString, JSAny, URL,
              alert, prompt, eval, writeLog, catJSStr, fromJSStr,
              module Haste.JSType, module Haste.DOM,
              module Haste.Callback, module Haste.Random,
              module Haste.Foreign) where
import Haste.Prim
import Haste.Callback
import Haste.Random
import Haste.JSType
import Haste.DOM
import Haste.Foreign
import Control.Monad.IO.Class
#ifndef __HASTE__
import Data.List (intercalate)
#endif

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

type URL = String

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
eval js = liftIO $ jsEval (toJSStr js) >>= return . fromJSStr

-- | Use console.log to write a message.
writeLog :: MonadIO m => String -> m ()
writeLog = liftIO . jsLog . toJSStr

-- | Concatenate a series of JSStrings using the specified separator.
catJSStr :: JSString -> [JSString] -> JSString
#ifdef __HASTE__
foreign import ccall jsCat    :: Ptr [JSString] -> JSString -> JSString
catJSStr sep strs = jsCat (toPtr strs) sep
#else
catJSStr sep strs = toJSStr $ intercalate (fromJSStr sep) (map fromJSStr strs)
#endif
