{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances,
             GADTs, CPP, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Basic Canvas graphics library.
module Haste.Graphics.Canvas (
    -- * Basic types and classes
    Bitmap, Canvas, Shape, Picture, Point, Vector, Angle, Rect (..), Color (..),
    Ctx, AnyImageBuffer (..),
    ImageBuffer (..), BitmapSource (..),

    -- *  Obtaining a canvas
    getCanvasById, getCanvas, createCanvas,

    -- * Rendering and reading canvases
    render, renderOnTop, renderOnTopBy, buffer, toDataURL, clearRect,

    -- * Colors and opacity
    setStrokeColor, setFillColor, color, opacity,

    -- * Matrix operations
    translate, scale, rotate,
    -- * Drawing shapes
    stroke, fill, clip,
    lineWidth, line, path, rect, circle, arc, quadraticCurve, bezierCurve,

    -- * Rendering text
    font, text,

    -- * Extending the library
    withContext
  ) where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import Haste
import Haste.DOM.JSString
import Haste.DOM.Core
import Haste.Concurrent (CIO) -- for SPECIALISE pragma
import Haste.Foreign (ToAny (..), FromAny (..), ffi)

jsHasCtx2D :: Elem -> IO Bool
jsHasCtx2D = ffi "(function(e){return !!e.getContext;})"

jsGetCtx2D :: Elem -> IO Ctx
jsGetCtx2D = ffi "(function(e){return e.getContext('2d');})"

jsBeginPath :: Ctx -> IO ()
jsBeginPath = ffi "(function(ctx){ctx.beginPath();})"

jsMoveTo :: Ctx -> Double -> Double -> IO ()
jsMoveTo = ffi "(function(ctx,x,y){ctx.moveTo(x,y);})"

jsLineTo :: Ctx -> Double -> Double -> IO ()
jsLineTo = ffi "(function(ctx,x,y){ctx.lineTo(x,y);})"

jsStroke :: Ctx -> IO ()
jsStroke = ffi "(function(ctx){ctx.stroke();})"

jsFill :: Ctx -> IO ()
jsFill = ffi "(function(ctx){ctx.fill();})"

jsRotate :: Ctx -> Double -> IO ()
jsRotate = ffi "(function(ctx,rad){ctx.rotate(rad);})"

jsTranslate :: Ctx -> Double -> Double -> IO ()
jsTranslate = ffi "(function(ctx,x,y){ctx.translate(x,y);})"

jsScale :: Ctx -> Double -> Double -> IO ()
jsScale = ffi "(function(ctx,x,y){ctx.scale(x,y);})"

jsPushState :: Ctx -> IO ()
jsPushState = ffi "(function(ctx){ctx.save();})"

jsPopState :: Ctx -> IO ()
jsPopState = ffi "(function(ctx){ctx.restore();})"

jsResetCanvas :: Elem -> IO ()
jsResetCanvas = ffi "(function(e){e.width = e.width;})"

jsClearRect :: Ctx
            -> Double -> Double
            -> Double -> Double
            -> IO ()
jsClearRect = ffi "(function(ctx,x,y,width,height){ctx.clearRect(x,y,width,height);})"

jsDrawImage :: Ctx -> Elem -> Double -> Double -> IO ()
jsDrawImage = ffi "(function(ctx,i,x,y){ctx.drawImage(i,x,y);})"

jsDrawImageClipped :: Ctx -> Elem
                   -> Double -> Double
                   -> Double -> Double -> Double -> Double
                   -> IO ()
jsDrawImageClipped = ffi "(function(ctx, img, x, y, cx, cy, cw, ch){\
ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);})"

jsDrawImageScaled :: Ctx -> Elem
                  -> Double -> Double -> Double -> Double
                  -> IO ()
jsDrawImageScaled = ffi "(function(ctx, img, x, y, w, h){\
ctx.drawImage(img, x, y, w, h);})"

jsSetGlobalCompositeOperation :: String -> IO ()
jsSetGlobalCompositeOperation = ffi "(function(s){globalCompositeOperation=s;})"

jsDrawText :: Ctx -> JSString -> Double -> Double -> IO ()
jsDrawText = ffi "(function(ctx,s,x,y){ctx.fillText(s,x,y);})"

jsClip :: Ctx -> IO ()
jsClip = ffi "(function(ctx){ctx.clip();})"

jsArc :: Ctx -> Double -> Double
             -> Double
             -> Double -> Double
             -> IO ()
jsArc = ffi "(function(ctx, x, y, radius, fromAngle, toAngle){\
ctx.arc(x, y, radius, fromAngle, toAngle);})"

jsQuadraticCurve :: Ctx
                 -> Double -> Double
                 -> Double -> Double
                 -> IO ()
jsQuadraticCurve = ffi "(function(ctx,cx,cy,x,y){ctx.quadraticCurveTo(cx,cy,x,y);})"

jsBezierCurve :: Ctx
                 -> Double -> Double
                 -> Double -> Double
                 -> Double -> Double
                 -> IO ()
jsBezierCurve = ffi "(function(ctx,c1x,c1y,c2x,c2y,x,y){ctx.bezierCurveTo(c1x,c1y,c2x,c2y,x,y);})"

jsCanvasToDataURL :: Elem -> IO JSString
jsCanvasToDataURL = ffi "(function(e){return e.toDataURL('image/png');})"

-- | A bitmap, backed by an IMG element.
--   JS representation is a reference to the backing IMG element.
newtype Bitmap = Bitmap Elem
  deriving (ToAny, FromAny)

-- | Any type that contains a buffered image which can be drawn onto a canvas.
class ImageBuffer a where
  -- | Draw the image buffer with its top left corner at the specified point.
  draw :: a -> Point -> Picture ()
  -- | Draw a portion of the image buffer with its top left corner at the
  --   specified point.
  drawClipped :: a -> Point -> Rect -> Picture ()
  -- | Draw the image buffer within given rectangle.
  drawScaled :: a -> Rect -> Picture ()

instance ImageBuffer Canvas where
  draw (Canvas _ buf) (x, y) = Picture $ \ctx -> jsDrawImage ctx buf x y
  drawClipped (Canvas _ buf) (x, y) (Rect cx cy cw ch) = Picture $ \ctx ->
    jsDrawImageClipped ctx buf x y cx cy cw ch
  drawScaled (Canvas _ buf) (Rect x y w h) = Picture $ \ctx ->
    jsDrawImageScaled ctx buf x y w h

instance ImageBuffer Bitmap where
  draw (Bitmap buf) (x, y) = Picture $ \ctx -> jsDrawImage ctx buf x y
  drawClipped (Bitmap buf) (x, y) (Rect cx cy cw ch) = Picture $ \ctx ->
    jsDrawImageClipped ctx buf x y cx cy cw ch
  drawScaled (Bitmap buf) (Rect x y w h) = Picture $ \ctx ->
    jsDrawImageScaled ctx buf x y w h

-- | Any type that can be used to obtain a bitmap.
class BitmapSource src where
  -- | Load a bitmap from some kind of bitmap source.
  loadBitmap :: MonadIO m => src -> m Bitmap

instance BitmapSource URL where
  loadBitmap url = liftIO $ do
    img <- newElem "img"
    setProp img "src" (toJSString url)
    loadBitmap img

instance BitmapSource Elem where
  loadBitmap = return . Bitmap

data AnyImageBuffer where
  AnyImageBuffer :: ImageBuffer a => a -> AnyImageBuffer

instance ImageBuffer AnyImageBuffer where
  draw (AnyImageBuffer buf) = draw buf
  drawClipped (AnyImageBuffer buf) = drawClipped buf
  drawScaled (AnyImageBuffer buf) = drawScaled buf

instance IsElem Canvas where
  elemOf (Canvas _ctx e) = e
  fromElem               = getCanvas

instance IsElem Bitmap where
  elemOf (Bitmap e) = e

-- | A point in the plane.
type Point = (Double, Double)

-- | A two dimensional vector.
type Vector = (Double, Double)

-- | An angle, given in radians.
type Angle = Double

-- | A rectangle.
data Rect = Rect {rect_x :: !Double,
                  rect_y :: !Double,
                  rect_w :: !Double,
                  rect_h :: !Double}

-- | A color, specified using its red, green and blue components, with an
--   optional alpha component.
data Color = RGB  !Int !Int !Int
           | RGBA !Int !Int !Int !Double

-- | Somewhat efficient conversion from Color to JSString.
color2JSString :: Color -> JSString
color2JSString (RGB r g b) =
  catJSStr "" ["rgb(", toJSString r, ",", toJSString g, ",", toJSString b, ")"]
color2JSString (RGBA r g b a) =
  catJSStr "" ["rgba(", toJSString r, ",",
                        toJSString g, ",",
                        toJSString b, ",",
                        toJSString a, ")"]

-- | Composition operations. For detailed explanation, see <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/globalCompositeOperation>
data CompositionOperation = Default
                          | SourceOver
                          | SourceIn
                          | SourceOut
                          | SourceAtop
                          | DestinationOver
                          | DestinationIn
                          | DestinationOut
                          | DestinationAtop
                          | Lighter
                          | Copy
                          | Xor
                          | Multiply
                          | Screen
                          | Overlay
                          | Darken
                          | Lighten
                          | ColorDodge
                          | ColorBurn
                          | HardLight
                          | SoftLight
                          | Difference
                          | Exclusion
                          | Hue
                          | Saturation
                          | Color
                          | Luminosity
                          deriving Eq

-- | A drawing context; part of a canvas.
--   JS representation is the drawing context object itself.
newtype Ctx = Ctx JSAny
  deriving (ToAny, FromAny)

-- | A canvas; a viewport into which a picture can be rendered.
--   The origin of the coordinate system used by the canvas is the top left
--   corner of the canvas element.
--   JS representation is a reference to the backing canvas element.
data Canvas = Canvas !Ctx !Elem

instance FromAny Canvas where
  fromAny c = do
    mcan <- fromAny c >>= fromElem
    case mcan of
      Just can -> return can
      _        -> error "Attempted to turn a non-canvas element into a Canvas!"

instance ToAny Canvas where
  toAny (Canvas _ el) = toAny el

-- | A picture that can be drawn onto a canvas.
newtype Picture a = Picture {unP :: Ctx -> IO a}

-- | A shape which can be either stroked or filled to yield a picture.
newtype Shape a = Shape {unS :: Ctx -> IO a}

instance Functor Picture where
  fmap f p = Picture $ \ctx ->
    unP p ctx >>= return . f

instance Applicative Picture where
  pure a = Picture $ \_ -> return a

  pfab <*> pa = Picture $ \ctx -> do
    fab <- unP pfab ctx
    a   <- unP pa   ctx
    return (fab a)

instance Monad Picture where
  return x = Picture $ \_ -> return x
  Picture m >>= f = Picture $ \ctx -> do
    x <- m ctx
    unP (f x) ctx

instance Functor Shape where
  fmap f s = Shape $ \ctx ->
    unS s ctx >>= return . f

instance Applicative Shape where
  pure a = Shape $ \_ -> return a

  sfab <*> sa = Shape $ \ctx -> do
    fab <- unS sfab ctx
    a   <- unS sa   ctx
    return (fab a)

instance Monad Shape where
  return x = Shape $ \_ -> return x
  Shape m >>= f = Shape $ \ctx -> do
    x <- m ctx
    unS (f x) ctx

-- | Create a 2D drawing context from a DOM element identified by its ID.
getCanvasById :: MonadIO m => String -> m (Maybe Canvas)
getCanvasById eid = liftIO $ do
  e <- elemById (toJSString eid)
  maybe (return Nothing) getCanvas e

{-# DEPRECATED getCanvas "use the more general fromElem instead." #-}
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
createCanvas :: Int -> Int -> IO Canvas
createCanvas w h = do
  buf <- newElem "canvas"
  setProp buf "width" (toJSString w)
  setProp buf "height" (toJSString h)
  fromJust <$> getCanvas buf

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
renderOnTop (Canvas ctx _) (Picture p) = liftIO $ p ctx

-- | Draw a picture onto a canvas without first clearing it, with a specific composition method
renderOnTopBy :: MonadIO m => Canvas -> Picture a -> CompositionOperation -> m a
renderOnTopBy canvas pic composition = liftIO $ do
    jsSetGlobalCompositeOperation $ translate composition
    rst <- renderOnTop canvas pic
    jsSetGlobalCompositeOperation $ translate Default
    return rst
        where
            translate :: CompositionOperation -> String
            translate c = fromJust $ lookup c dict

            dict = [
                (Default, "source-over"),
                (SourceOver, "source-over"),
                (SourceIn, "source-in"),
                (SourceOut, "source-out"),
                (SourceAtop, "source-atop"),
                (DestinationOver, "destination-over"),
                (DestinationIn, "destination-in"),
                (DestinationOut, "destination-out"),
                (DestinationAtop, "destination-atop"),
                (Lighter, "lighter"),
                (Copy, "copy"),
                (Xor, "xor"),
                (Multiply, "multiply"),
                (Screen, "screen"),
                (Overlay, "overlay"),
                (Darken, "darken"),
                (Lighten, "Lighten"),
                (ColorDodge, "color-dodge"),
                (ColorBurn, "color-burn"),
                (HardLight, "hard-light"),
                (SoftLight, "soft-light"),
                (Difference, "difference"),
                (Exclusion, "exclusion"),
                (Hue, "hue"),
                (Saturation, "saturation"),
                (Color, "color"),
                (Luminosity, "luminosity")
                ]
                -- It may actually run faster, if the lookuping is implemented in js. But for purity...

-- | Clear the rectangular area between the given two points.
clearRect :: Point -> Point -> Picture ()
clearRect (x1, y1) (x2, y2) = Picture $ \ctx -> jsClearRect ctx x1 y1 (x2-x1) (y2-y1)

-- | Generate a data URL from the contents of a canvas.
toDataURL :: MonadIO m => Canvas -> m URL
toDataURL (Canvas _ el) = liftIO $ do
  fromJSStr <$> jsCanvasToDataURL el

-- | Create a new off-screen buffer and store the given picture in it.
buffer :: MonadIO m => Int -> Int -> Picture () -> m Bitmap
buffer w h pict = liftIO $ do
  buf@(Canvas _ el) <- createCanvas w h
  render buf pict
  return $ Bitmap el

-- | Perform a computation over the drawing context of the picture.
--   This is handy for operations which are either impossible, hard or
--   inefficient to express using the Haste.Graphics.Canvas API.
withContext :: (Ctx -> IO a) -> Picture a
withContext f = Picture $ \ctx -> f ctx

-- | Set a new color for strokes.
setStrokeColor :: Color -> Picture ()
setStrokeColor c = Picture $ \(Ctx ctx) -> do
  setProp (Elem ctx) "strokeStyle" (color2JSString c)

-- | Set a new fill color.
setFillColor :: Color -> Picture ()
setFillColor c = Picture $ \(Ctx ctx) -> do
  setProp (Elem ctx) "fillStyle" (color2JSString c)

-- | Draw a picture with the given opacity.
opacity :: Double -> Picture () -> Picture ()
opacity alpha (Picture pict) = Picture $ \(Ctx ctx) -> do
  alpha' <- getProp (Elem ctx) "globalAlpha"
  setProp (Elem ctx) "globalAlpha" (toJSString alpha)
  pict (Ctx ctx)
  setProp (Elem ctx) "globalAlpha" alpha'

-- | Draw the given Picture using the specified Color for both stroke and fill,
--   then restore the previous stroke and fill colors.
color :: Color -> Picture () -> Picture ()
color c (Picture pict) = Picture $ \(Ctx ctx) -> do
    fc <- getProp (Elem ctx) "fillStyle"
    sc <- getProp (Elem ctx) "strokeStyle"
    setProp (Elem ctx) "fillStyle" c'
    setProp (Elem ctx) "strokeStyle" c'
    pict (Ctx ctx)
    setProp (Elem ctx) "fillStyle" fc
    setProp (Elem ctx) "strokeStyle" sc
  where
    c' = color2JSString c

-- | Draw the given picture using a new line width.
lineWidth :: Double -> Picture () -> Picture ()
lineWidth w (Picture pict) = Picture $ \(Ctx ctx) -> do
  lw <- getProp (Elem ctx) "lineWidth"
  setProp (Elem ctx) "lineWidth" (toJSString w)
  pict (Ctx ctx)
  setProp (Elem ctx) "lineWidth" lw

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

-- | Draw a quadratic Bezier curve.
quadraticCurve ::
    Point -- ^ Start point.
    -> Point -- ^ End point.
    -> Point -- ^ Control point.
    -> Shape ()
quadraticCurve (sx, sy) (ex, ey) (cx, cy) = Shape $ \ctx -> do
    jsMoveTo ctx sx sy
    jsQuadraticCurve ctx cx cy ex ey

-- | Draw a Bezier curve.
bezierCurve ::
    Point -- ^ Start point.
    -> Point -- ^ End point.
    -> Point -- ^ Start control point.
    -> Point -- ^ End control point.
    -> Shape ()
bezierCurve (sx, sy) (ex, ey) (scx, scy) (ecx, ecy) = Shape $ \ctx -> do
    jsMoveTo ctx sx sy
    jsBezierCurve ctx scx scy ecx ecy ex ey

-- | Draw a picture using a certain font. Obviously only affects text.
font :: String -> Picture () -> Picture ()
font f (Picture pict) = Picture $ \(Ctx ctx) -> do
  f' <- getProp (Elem ctx) "font"
  setProp (Elem ctx) "font" (toJSString f)
  pict (Ctx ctx)
  setProp (Elem ctx) "font" f'

-- | Draw some text onto the canvas.
text :: Point -> String -> Picture ()
text (x, y) str = Picture $ \ctx -> jsDrawText ctx (toJSString str) x y
