{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
-- | Haste.App client-server protocol.
module Haste.App.Protocol where
import Control.Applicative
import Control.Exception
import Data.Typeable
import Haste.Binary

type Nonce = Int
type CallID = Int

-- | A method call to the server.
data ServerCall = ServerCall {
    scNonce  :: Nonce,
    scMethod :: CallID,
    scArgs   :: [Blob]
  }

instance Binary ServerCall where
  {-# NOINLINE get #-}
  get = do
    n <- getWord8
    if n == 0
      then ServerCall <$> get <*> get <*> get
      else fail "Wrong magic byte for ServerCall"
  put (ServerCall n c as) = putWord8 0 >> put n >> put c >> put as

-- | A reply to a ServerCall.
data ServerReply = ServerReply {
    srNonce  :: Nonce,
    srResult :: Blob
  }

instance Binary ServerReply where
  {-# NOINLINE get #-}
  get = do
    n <- getWord8
    if n == 1
      then ServerReply <$> get <*> get
      else fail "Wrong magic byte for ServerReply"
  put (ServerReply n r) = putWord8 1 >> put n >> put r

-- | Throw a server exception to the client.
data ServerException = ServerException String deriving (Typeable, Show)
instance Exception ServerException

instance Binary ServerException where
  {-# NOINLINE get #-}
  get = do
    n <- getWord8
    if n == 2
      then ServerException <$> get
      else fail "Wrong magic byte for ServerException"
  put (ServerException e) = putWord8 2 >> put e
