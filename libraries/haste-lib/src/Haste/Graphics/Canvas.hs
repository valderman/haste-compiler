{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings,
             TypeSynonymInstances, FlexibleInstances, GADTs, CPP #-}
-- | Basic Canvas graphics library.
module Haste.Graphics.Canvas (
  -- Types
  Bitmap, Canvas, Shape, Picture, Point, Vector, Angle, Rect (..), Color (..),
  AnyImageBuffer (..),
  -- Classes
  ImageBuffer (..), BitmapSource (..),
  -- Obtaining a canvas for drawing
  getCanvasById, getCanvas, createCanvas,
  -- Working with bitmaps
  bitmapElem,
  -- Rendering pictures, extracting data from a canvas
  render, renderOnTop, buffer, toDataURL,
  -- Working with colors and opacity
  setStrokeColor, setFillColor, color, opacity,
  -- Matrix operations
  translate, scale, rotate,
  -- Using shapes
  stroke, fill, clip,
  -- Creating shapes
  line, path, rect, circle, arc,
  -- Working with text
  font, text
  ) where
import Control.Applicative
import Control.Monad.IO.Class
import Haste
import Haste.Concurrent (CIO) -- for SPECIALISE pragma

#ifdef __HASTE__
foreign import ccall jsHasCtx2D :: Elem -> IO Bool
foreign import ccall jsGetCtx2D :: Elem -> IO Ctx
foreign import ccall jsBeginPath :: Ctx -> IO ()
foreign import ccall jsMoveTo :: Ctx -> Double -> Double -> IO ()
foreign import ccall jsLineTo :: Ctx -> Double -> Double -> IO ()
foreign import ccall jsStroke :: Ctx -> IO ()
foreign import ccall jsFill :: Ctx -> IO ()
foreign import ccall jsRotate :: Ctx -> Double -> IO ()
foreign import ccall jsTranslate :: Ctx -> Double -> Double -> IO ()
foreign import ccall jsScale :: Ctx -> Double -> Double -> IO ()
foreign import ccall jsPushState :: Ctx -> IO ()
foreign import ccall jsPopState :: Ctx -> IO ()
foreign import ccall jsResetCanvas :: Elem -> IO ()
foreign import ccall jsDrawImage :: Ctx -> Elem -> Double -> Double -> IO ()
foreign import ccall jsDrawImageClipped :: Ctx -> Elem
                                        -> Double -> Double
                                        -> Double -> Double -> Double -> Double 
                                        -> IO ()
foreign import ccall jsDrawText :: Ctx -> JSString -> Double -> Double -> IO ()
foreign import ccall jsClip :: Ctx -> IO ()
foreign import ccall jsArc :: Ctx
                           -> Double -> Double
                           -> Double
                           -> Double -> Double
                           -> IO ()
foreign import ccall jsCanvasToDataURL :: Elem -> IO JSString
#else
jsHasCtx2D = error "Tried to use Canvas in native code!"
jsGetCtx2D = error "Tried to use Canvas in native code!"
jsBeginPath = error "Tried to use Canvas in native code!"
jsMoveTo = error "Tried to use Canvas in native code!"
jsLineTo = error "Tried to use Canvas in native code!"
jsStroke = error "Tried to use Canvas in native code!"
jsFill = error "Tried to use Canvas in native code!"
jsRotate = error "Tried to use Canvas in native code!"
jsTranslate = error "Tried to use Canvas in native code!"
jsScale = error "Tried to use Canvas in native code!"
jsPushState = error "Tried to use Canvas in native code!"
jsPopState = error "Tried to use Canvas in native code!"
jsResetCanvas = error "Tried to use Canvas in native code!"
jsDrawImage = error "Tried to use Canvas in native code!"
jsDrawImageClipped = error "Tried to use Canvas in native code!"
jsDrawText = error "Tried to use Canvas in native code!"
jsClip = error "Tried to use Canvas in native code!"
jsArc = error "Tried to use Canvas in native code!"
jsCanvasToDataURL = error "Tried to use Canvas in native code!"
#endif

newtype Bitmap = Bitmap Elem

-- | Any type that contains a buffered image which can be drawn onto a canvas.
class ImageBuffer a where
  -- | Draw the image buffer with its top left corner at the specified point.
  draw :: a -> Point -> Picture ()
  -- | Draw a portion of the image buffer with its top left corner at the
  --   specified point.
  drawClipped :: a -> Point -> Rect -> Picture ()

instance ImageBuffer Canvas where
  draw (Canvas _ buf) (x, y) = Picture $ \ctx -> jsDrawImage ctx buf x y
  drawClipped (Canvas _ buf) (x, y) (Rect cx cy cw ch) = Picture $ \ctx ->
    jsDrawImageClipped ctx buf x y cx cy cw ch

instance ImageBuffer Bitmap where
  draw (Bitmap buf) (x, y) = Picture $ \ctx -> jsDrawImage ctx buf x y
  drawClipped (Bitmap buf) (x, y) (Rect cx cy cw ch) = Picture $ \ctx ->
    jsDrawImageClipped ctx buf x y cx cy cw ch

-- | Any type that can be used to obtain a bitmap.
class BitmapSource src where
  -- | Load a bitmap from some kind of bitmap source.
  loadBitmap :: MonadIO m => src -> m Bitmap

instance BitmapSource URL where
  loadBitmap url = liftIO $ do
    img <- newElem "img"
    setProp img "src" url
    loadBitmap img

instance BitmapSource Elem where
  loadBitmap = return . Bitmap

data AnyImageBuffer where
  AnyImageBuffer :: ImageBuffer a => a -> AnyImageBuffer

instance ImageBuffer AnyImageBuffer where
  draw (AnyImageBuffer buf) = draw buf
  drawClipped (AnyImageBuffer buf) = drawClipped buf

-- | Get the HTML element associated with the given bitmap.
bitmapElem :: Bitmap -> Elem
bitmapElem (Bitmap e) = e

-- | A point in the plane.
type Point = (Double, Double)

-- | A two dimensional vector.
type Vector = (Double, Double)

-- | An angle, given in radians.
type Angle = Double

-- | A rectangle.
data Rect = Rect {rect_x :: Double,
                  rect_y :: Double,
                  rect_w :: Double,
                  rect_h :: Double}

-- | A color, specified using its red, green and blue components, with an
--   optional alpha component.
data Color = RGB Int Int Int
           | RGBA Int Int Int Double

-- | Somewhat efficient conversion from Color to JSString.
color2JSString :: Color -> JSString
color2JSString (RGB r g b) =
  catJSStr "" ["rgb(", toJSString r, ",", toJSString g, ",", toJSString b, ")"]
color2JSString (RGBA r g b a) =
  catJSStr "" ["rgba(", toJSString r, ",",
                        toJSString g, ",",
                        toJSString b, ",",
                        toJSString a, ")"]

-- | A drawing context; part of a canvas.
newtype Ctx = Ctx JSAny

-- | A canvas; a viewport into which a picture can be rendered.
--   The origin of the coordinate system used by the canvas is the top left
--   corner of the canvas element.
data Canvas = Canvas Ctx Elem

-- | A picture that can be drawn onto a canvas.
newtype Picture a = Picture {unP :: Ctx -> IO a}

-- | A shape which can be either stroked or filled to yield a picture.
newtype Shape a = Shape {unS :: Ctx -> IO a}

instance Monad Picture where
  return x = Picture $ \_ -> return x
  Picture m >>= f = Picture $ \ctx -> do
    x <- m ctx
    unP (f x) ctx

instance Monad Shape where
  return x = Shape $ \_ -> return x
  Shape m >>= f = Shape $ \ctx -> do
    x <- m ctx
    unS (f x) ctx

-- | Create a 2D drawing context from a DOM element identified by its ID.
getCanvasById :: MonadIO m => ElemID -> m (Maybe Canvas)
getCanvasById eid = liftIO $ do
  e <- elemById eid
  maybe (return Nothing) getCanvas e

-- | Create a 2D drawing context from a DOM element.
getCanvas :: MonadIO m => Elem -> m (Maybe Canvas)
getCanvas e = liftIO $ do 
  hasCtx <- jsHasCtx2D e
  case hasCtx of
    True -> do
      ctx <- jsGetCtx2D e
      return $ Just $ Canvas ctx e
    _    -> return Nothing

-- | Create an off-screen buffer of the specified size.
createCanvas :: Int -> Int -> IO (Maybe Canvas)
createCanvas w h = do
  buf <- newElem "canvas"
  setProp buf "width" (toString w)
  setProp buf "height" (toString h)
  getCanvas buf

-- | Clear a canvas, then draw a picture onto it.
{-# SPECIALISE render :: Canvas -> Picture a -> IO a #-}
{-# SPECIALISE render :: Canvas -> Picture a -> CIO a #-}
render :: MonadIO m => Canvas -> Picture a -> m a
render (Canvas ctx el) (Picture p) = liftIO $ do
  jsResetCanvas el
  p ctx

-- | Draw a picture onto a canvas without first clearing it.
{-# SPECIALISE renderOnTop :: Canvas -> Picture a -> IO a #-}
{-# SPECIALISE renderOnTop :: Canvas -> Picture a -> CIO a #-}
renderOnTop :: MonadIO m => Canvas -> Picture a -> m a
renderOnTop (Canvas ctx el) (Picture p) = liftIO $ p ctx

-- | Generate a data URL from the contents of a canvas.
toDataURL :: MonadIO m => Canvas -> m URL
toDataURL (Canvas _ el) = liftIO $ do
  fromJSStr <$> jsCanvasToDataURL el

-- | Create a new off-screen buffer and store the given picture in it.
buffer :: MonadIO m => Int -> Int -> Picture () -> m Bitmap
buffer w h pict = liftIO $ do
  mbuf <- createCanvas w h
  case mbuf of
    Just buf@(Canvas _ el) -> do
      render buf pict
      return $ Bitmap el
    _ -> do
      Bitmap <$> newElem "img"

-- | Set a new color for strokes.
setStrokeColor :: Color -> Picture ()
setStrokeColor c = Picture $ \(Ctx ctx) -> do
  setProp' (Elem ctx) "strokeStyle" (color2JSString c)

-- | Set a new fill color.
setFillColor :: Color -> Picture ()
setFillColor c = Picture $ \(Ctx ctx) -> do
  setProp' (Elem ctx) "fillStyle" (color2JSString c)

-- | Draw a picture with the given opacity.
opacity :: Double -> Picture () -> Picture ()
opacity alpha (Picture pict) = Picture $ \(Ctx ctx) -> do
  alpha' <- getProp' (Elem ctx) "globalAlpha"
  setProp' (Elem ctx) "globalAlpha" (toJSString alpha)
  pict (Ctx ctx)
  setProp' (Elem ctx) "globalAlpha" alpha'

-- | Draw the given Picture using the specified Color for both stroke and fill,
--   then restore the previous stroke and fill colors.
color :: Color -> Picture () -> Picture ()
color c (Picture pict) = Picture $ \(Ctx ctx) -> do
    fc <- getProp' (Elem ctx) "fillStyle"
    sc <- getProp' (Elem ctx) "strokeStyle"
    setProp' (Elem ctx) "fillStyle" c'
    setProp' (Elem ctx) "strokeStyle" c'
    pict (Ctx ctx)
    setProp' (Elem ctx) "fillStyle" fc
    setProp' (Elem ctx) "strokeStyle" sc
  where
    c' = color2JSString c

-- | Draw the specified picture using the given point as the origin.
translate :: Vector -> Picture () -> Picture ()
translate (x, y) (Picture pict) = Picture $ \ctx -> do
  jsPushState ctx
  jsTranslate ctx x y
  pict ctx
  jsPopState ctx

-- | Draw the specified picture rotated @r@ radians clockwise.
rotate :: Double -> Picture () -> Picture ()
rotate rad (Picture pict) = Picture $ \ctx -> do
  jsPushState ctx
  jsRotate ctx rad
  pict ctx
  jsPopState ctx

-- | Draw the specified picture scaled as specified by the scale vector.
scale :: Vector -> Picture () -> Picture ()
scale (x, y) (Picture pict) = Picture $ \ctx -> do
  jsPushState ctx
  jsScale ctx x y
  pict ctx
  jsPopState ctx

-- | Draw a filled shape.
fill :: Shape () -> Picture ()
fill (Shape shape) = Picture $ \ctx -> do
  jsBeginPath ctx
  shape ctx
  jsFill ctx
  
-- | Draw the contours of a shape.
stroke :: Shape () -> Picture ()
stroke (Shape shape) = Picture $ \ctx -> do
  jsBeginPath ctx
  shape ctx
  jsStroke ctx

-- | Draw a picture clipped to the given path.
clip :: Shape () -> Picture () -> Picture ()
clip (Shape shape) (Picture pict) = Picture $ \ctx -> do
  jsPushState ctx
  jsBeginPath ctx
  shape ctx
  jsClip ctx
  pict ctx
  jsPopState ctx

-- | Draw a path along the specified points.
path :: [Point] -> Shape ()
path ((x1, y1):ps) = Shape $ \ctx -> do
  jsMoveTo ctx x1 y1
  mapM_ (uncurry $ jsLineTo ctx) ps
path _ =
  return ()

-- | Draw a line between two points.
line :: Point -> Point -> Shape ()
line p1 p2 = path [p1, p2]

-- | Draw a rectangle between the two given points.
rect :: Point -> Point -> Shape ()
rect (x1, y1) (x2, y2) = path [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)]

-- | Draw a circle shape.
circle :: Point -> Double -> Shape ()
circle (x, y) radius = Shape $ \ctx -> do
  jsMoveTo ctx (x+radius) y
  jsArc ctx x y radius (0 :: Double) twoPi

{-# INLINE twoPi #-}
twoPi :: Double
twoPi = 2*pi

-- | Draw an arc. An arc is specified as a drawn portion of an imaginary
--   circle with a center point, a radius, a starting angle and an ending
--   angle.
--   For instance, @arc (0, 0) 10 0 pi@ will draw a half circle centered at
--   (0, 0), with a radius of 10 pixels.
arc :: Point -> Double -> Angle -> Angle -> Shape ()
arc (x, y) radius from to = Shape $ \ctx -> jsArc ctx x y radius from to

-- | Draw a picture using a certain font. Obviously only affects text.
font :: String -> Picture () -> Picture ()
font f (Picture pict) = Picture $ \(Ctx ctx) -> do
  f' <- getProp' (Elem ctx) "font"
  setProp' (Elem ctx) "font" (toJSString f)
  pict (Ctx ctx)
  setProp' (Elem ctx) "font" f'

-- | Draw some text onto the canvas.
text :: Point -> String -> Picture ()
text (x, y) str = Picture $ \ctx -> jsDrawText ctx (toJSString str) x y
