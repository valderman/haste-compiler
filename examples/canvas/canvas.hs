-- Example of using the Canvas library to render simple tile maps.
import Haste
import Haste.Graphics.Canvas
import Data.IORef
import Data.Array
import Data.Array.MArray
import Data.Array.IO

-- | A map contains of a tileset, dimensions and tile data.
data Tilemap = Tilemap {
    tileData   :: Array Int Int,
    tileSet    :: Bitmap,
    dimensions :: (Int, Int)
  }

-- | The state of the program: which tile is currently selected, and what does
--   the map look like?
data State = State {
    tileMap :: Tilemap,
    selectedTile :: (Int, Int)
  }

-- | Draw a single tile.
--   The tileset is assumed to consist of 11*11 tiles of 16*16 pixels each,
--   separated by a one pixel border.
drawTile :: Bitmap -> Int -> Point -> Picture ()
drawTile tileset tile pt = do
    drawClipped tileset pt (Rect (1+tx*17) (1+ty*17) 16 16)
  where
    (ty, tx) = quotRem tile 11

-- | Update a tile in a tile map. This function is horribly slow, copying the
--   entire map *twice* for each overwrite.
writeTile :: Tilemap -> (Int, Int) -> Int -> IO Tilemap
writeTile m@(Tilemap _ _ (w, _)) (x, y) t = do
  arr <- thaw (tileData m) :: IO (IOArray Int Int)
  writeArray arr (y*w+x) t
  arr' <- freeze arr
  return $ m {tileData = arr'}

-- | Draw an entire tile map.
drawMap :: Tilemap -> Picture ()
drawMap (Tilemap m ts (w, h)) = do
  sequence_ [drawTile ts (m ! (y*w+x)) (x*16, y*16) |
             x <- [0 .. w-1], y <- [0 .. h-1]]

-- | Draw a tilemap, then draw a semi-transparent square over the tile that's
--   currently selected.
drawMapWithSel :: Tilemap -> (Int, Int) -> Picture ()
drawMapWithSel tilemap (x, y) = do
    drawMap tilemap
    color (RGBA 255 255 255 0.5) . fill $ rect (x', y') (x'+16, y'+16)
  where
    x' = x*16
    y' = y*16

-- | Create a tilemap using the specified tileset, dimensions and default tile.
createMap :: Bitmap -> Int -> Int -> Int -> Tilemap
createMap tileset w h deftile = Tilemap {
    tileData   = listArray (0, w*h-1) $ replicate (w*h) deftile,
    tileSet    = tileset,
    dimensions = (w, h)
  }

main :: IO ()
main = do
  -- Setup: get the HTML element for a canvas, then proceed to create a canvas
  -- object from it.
  Just ce <- elemById "canvas"
  Just c <- getCanvas ce
  
  -- Same for the canvas we're using as the tile "toolbox"
  Just tilemapElem <- elemById "tiles"
  Just tiles <- getCanvas tilemapElem
  
  -- Load our tileset from a PNG file.
  tileset <- loadBitmap "tileset.png"

  -- The tile toolbox is a tilemap in itself.
  let allTiles = Tilemap {
          tileData   = listArray (0, 120) [0..],
          tileSet    = tileset,
          dimensions = (11, 11)
        }
  
  -- Start the application with the top left element selected and a 10*10 map.
  state <- newIORef $ State {
      selectedTile = (0, 0),
      tileMap      = createMap tileset 10 10 12
    }

  -- When the user clicks the toolbox, mark the clicked tile as selected.
  tilemapElem `onEvent` OnClick $ \_button (x, y) -> do
    modifyIORef state $ \st ->
      st {selectedTile = (x `quot` 32, y `quot` 32)}
    readIORef state >>= drawEverything allTiles tiles c

  -- When the user clicks the map, overwrite the clicked tile with the
  -- currently selected one.
  ce `onEvent` OnClick $ \_button (x, y) -> do
    st <- readIORef state
    let (tx, ty) = selectedTile st
        tile = ty*11 + tx
    tmap <- writeTile (tileMap st) (x `quot` 32, y `quot` 32) tile
    let st' = st {tileMap = tmap}
    writeIORef state st'
    drawEverything allTiles tiles c st'

  -- Display a "brb loading" message if the user is on a slow connection
  render c $ text (110, 120) "Loading, please wait..."
  
  -- When the tileset finished loading, draw everything.
  bitmapElem tileset `onEvent` OnLoad $ do
    readIORef state >>= drawEverything allTiles tiles c
  return ()

-- | Render both of our tile maps, then extract the image data from the map
--   as a data URL and write it to the "share your creation" text box.
drawEverything :: Tilemap -> Canvas -> Canvas -> State -> IO ()
drawEverything allTiles tiles c st = do
  render tiles $ scale (2, 2) $ do
    drawMapWithSel allTiles (selectedTile st)
  render c $ scale (2, 2) $ do
    drawMap (tileMap st)
  Just dataurl <- elemById "dataurl"
  toDataURL c >>= setProp dataurl "value"
