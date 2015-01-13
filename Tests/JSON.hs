{-# LANGUAGE OverloadedStrings #-}
module Tests.JSON where
import Haste
import Haste.JSON
import Haste.Serialize
import Control.Applicative

data ABC = A | B | C
  deriving (Show, Read)

data Foo = Foo {
    string :: String,
    number :: Int,
    bool   :: Bool,
    nested :: Either String Foo,
    enum   :: ABC
  } deriving Show

instance Serialize ABC where
  toJSON = Str . toJSString . show
  parseJSON (Str s) = case fromJSString s of Just s' -> return $ read s'

instance Serialize Foo where
  toJSON (Foo s n b nest e) = Dict [
      ("string", toJSON s),
      ("number", toJSON n),
      ("bool", toJSON b),
      ("nested", toJSON nest),
      ("enum", toJSON e)
    ]
  parseJSON j = Foo <$> j .: "string"
                    <*> j .: "number"
                    <*> j .: "bool"
                    <*> j .: "nested"
                    <*> j .: "enum"                   

foo :: Foo
foo = Foo {
    string = "\"hälsena\"",
    number = -42,
    bool   = False,
    nested = Right (foo {nested = Left "nope"}),
    enum   = B
  }

fooStr :: JSString
fooStr = encodeJSON $ toJSON foo

fooAgain :: Either String Foo
fooAgain = decodeJSON fooStr >>= fromJSON

runTest :: IO ()
runTest = do
  print foo
  putStrLn ""
  print fooStr
  print $ decodeJSON fooStr
  putStrLn ""
  print fooAgain
