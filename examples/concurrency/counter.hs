module Main where
import Haste
import Haste.DOM
import Haste.Concurrent

main = concurrent $ do
  ctr <- newEmptyMVar
  forkIO $ updater ctr
  counter ctr 0

-- | This thread counts.
counter :: MVar Int -> Int -> CIO ()
counter ctr startval =
    go startval
  where
    go n = do
      putMVar ctr n
      wait 1000
      go (n+1)

-- | This thread updates the GUI.
updater :: MVar Int -> CIO ()
updater ctr =
    withElem "counter" $ go
  where
    go e = do
      x <- takeMVar ctr
      setProp e "innerHTML" (show x)
      go e
