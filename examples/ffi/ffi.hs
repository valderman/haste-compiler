{-# LANGUAGE OverloadedStrings #-}
-- | This example implements a really simple JQuery wrapper, to demonstrate
--   how foreign imports work.
module Main where
import Haste hiding (click)
import Haste.Foreign
import Control.Monad (when)
import Control.Applicative

newtype JQuery = JQuery JSAny
type Selector = String

data MouseEvent = MouseEvent Int

instance FromAny MouseEvent where
  fromAny x = MouseEvent <$> get x "button"

-- | The @ffi@ function allows you to import arbitrary JS code as a Haskell
--   function, provided that its arguments are all marshallable.
clicked :: Selector -> (MouseEvent -> IO ()) -> IO ()
clicked = ffi "(function(sel, f){$(sel).click(f);})"

hide :: Selector -> IO ()
hide = ffi "(function(sel){$(sel).hide();})"

main = do
  "#closeBlack" `clicked` \(MouseEvent button) -> do
    when (button == 0) (hide "#blackBox")
  "#closeRed" `clicked` \(MouseEvent button) -> do
    when (button == 0) (hide "#redBox")
