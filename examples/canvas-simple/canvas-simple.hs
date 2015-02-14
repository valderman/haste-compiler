import Haste
import Haste.Graphics.Canvas

-- | A 40*40 square with a 20*20 square inside of it and a line running
--   through it.
squareShape :: Shape ()
squareShape = do
  rect (-20, -20) (20, 20)
  rect (-10, -10) (10, 10)
  line (-20, -20) (20, 20)

-- | You can stroke any shape to get a "wireframe" version of them.
square :: Picture ()
square = stroke squareShape

-- | Or you can fill them.
filledSquare :: Picture ()
filledSquare = fill squareShape

-- | Then you grab a canvas object...
main :: IO ()
main = do
  Just can <- getCanvasById "canvas"
  animate can 0

-- | ...and use the render function to draw your image.
--   The picture type is a monad, so you can compose several pictures easily
--   using do-notation.
animate :: Canvas -> Double -> IO ()
animate can angle = do
  -- There are several transformation functions as well. All of them take a
  -- Picture () as their argument, and apply their transformation only to that
  -- picture, so the user doesn't need to manage the canvas state machine
  -- explicitly.
  render can $ do
    translate (160, 160) $ rotate angle $ do
      square
      translate (100, 100) . rotate (-angle) . color (RGB 255 0 0) $ filledSquare
    color (RGBA 0 0 255 0.5) . font "20px Bitstream Vera" $ do
      text (10, 160) "You can use transparency too!"
  setTimer (Once 10) $ animate can (angle + 0.01)
  return ()
