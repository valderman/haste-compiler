{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
-- | Haste.App client-server protocol.
module Haste.App.Protocol where
import Control.Applicative
import Control.Exception
import Data.Typeable
import Haste.Serialize
import Haste.JSON

type Nonce = Int
type CallID = Int

-- | A method call to the server.
data ServerCall = ServerCall {
    scNonce  :: Nonce,
    scMethod :: CallID,
    scArgs   :: [JSON]
  }

instance Serialize ServerCall where
  toJSON (ServerCall nonce method args) = Dict [
      ("nonce", toJSON nonce),
      ("method", toJSON method),
      ("args", Arr args)
    ]
  parseJSON d = do
    ServerCall <$> (d .: "nonce")
               <*> (d .: "method")
               <*> (d .: "args")

-- | A reply to a ServerCall.
data ServerReply = ServerReply {
    srNonce  :: Nonce,
    srResult :: JSON
  }

instance Serialize ServerReply where
  toJSON (ServerReply nonce result) = Dict [
      ("nonce", toJSON nonce),
      ("result", result)
    ]
  parseJSON d = do
    ServerReply <$> (d .: "nonce")
                <*> (d .: "result")

-- | Throw a server exception to the client.
data ServerException = ServerException String deriving (Typeable, Show)
instance Exception ServerException

instance Serialize ServerException where
  toJSON (ServerException s) = Dict [
      ("exception", toJSON True),
      ("message", toJSON s)
    ]

  parseJSON d = do
    mex <- d .:? "exception"
    case mex of
      Just True -> ServerException <$> (d .: "message")
      _         -> fail "Not a ServerException"
