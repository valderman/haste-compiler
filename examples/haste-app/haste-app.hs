-- | A simple example of using Haste.App: a "message trader".
--   When the button is clicked, the user is asked to input a message. This
--   message is then sent to the server, and the user in return receives the
--   previous message left using this trade function.
--
--   This example demonstrates the basics of using Haste.App: how to maintain
--   server state, and how to export and call server API functions.
import Haste.App
import qualified Haste.App.Concurrent as H
import qualified Control.Concurrent as C

main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    message <- liftServerIO $ C.newMVar "This is not a message."

    trade <- export $ \newmsg -> do
      message <- mkUseful message
      liftIO $ do oldmsg <- C.takeMVar message
                  C.putMVar message newmsg
                  return oldmsg

    runClient $ withElem "button" $ \button -> do
      button `onEvent` OnClick $ \_ _ -> do
        newmsg <- prompt "Enter a message"
        oldmsg <- onServer $ trade <.> newmsg
        alert $ "The old message was: " ++ oldmsg
