{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
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

foreign import ccall jsAlert  :: JSString -> IO ()
foreign import ccall jsLog    :: JSString -> IO ()
foreign import ccall jsPrompt :: JSString -> IO JSString
foreign import ccall jsEval   :: JSString -> IO JSString
foreign import ccall jsCat    :: Ptr [JSString] -> JSString -> JSString

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
catJSStr sep strs = jsCat (toPtr strs) sep
