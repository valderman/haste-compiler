{-# LANGUAGE OverloadedStrings #-}
-- | XHaste client-server protocol.
module XHaste.Protocol where
import Control.Applicative
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
  fromJSON d = do
    ServerCall <$> (liftMaybe (d ~> "nonce") >>= fromJSON)
               <*> (liftMaybe (d ~> "method") >>= fromJSON)
               <*> case d ~> "args" of
                     Just (Arr args') -> pure args'
                     _                -> fail "Args not an array!"

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
  fromJSON d = do
    ServerReply <$> (liftMaybe (d ~> "nonce") >>= fromJSON)
                <*> liftMaybe (d ~> "result")

liftMaybe :: Maybe a -> Either String a
liftMaybe (Just x) = Right x
liftMaybe _        = Left "liftMaybe failed"
