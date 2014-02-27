-- | A simple chatbox application using Haste.App.
--   While this example could be considerably shorter, the API calls are broken
--   out to demonstrate how one might want to pass them around in a larger
--   program.
import Haste.App
import Haste.App.Concurrent
import qualified Control.Concurrent as C
import Control.Monad
import Control.Applicative
import Data.List (lookup)
import Data.IORef
import qualified Data.Set as S

-- | A chat message consists of a sender name and a message.
type Message = (String, String)

-- | The type representing our state - a list matching active clients with
--   the MVars used to notify them of a new message, and a backlog of messages.
type State = (IORef [(SessionID, C.MVar Message)], IORef [Message])

-- | Data type to hold all our API calls
data API = API {
    apiHello :: Export (Server [Message]),
    apiSend :: Export (String -> String -> Server ()),
    apiAwait :: Export (Server Message)
  }

-- | Tell the server we're here and remove any stale sessions.
hello :: Useless State -> Server [Message]
hello state = do
  sid <- getSessionID
  active <- getActiveSessions
  (clients, messages) <- mkUseful state
  liftIO $ do
    v <- C.newEmptyMVar
    atomicModifyIORef clients $ \cs ->
      ((sid, v) : filter (\(sess, _) -> sess `S.member` active) cs, ())
    readIORef messages

-- | Send a message; keep a backlog of 100 messages.
send :: Useless State -> String -> String -> Server ()
send state sender msg = do
  (clients, messages) <- mkUseful state
  liftIO $ do
    cs <- readIORef clients
    atomicModifyIORef messages $ \msgs -> ((sender, msg):take 99 msgs, ())
    -- Fork a new thread for each MVar so slow clients don't hold up fast ones.
    forM_ cs $ \(_, v) -> C.forkIO $ C.putMVar v (sender, msg)

-- | Block until a new message arrives, then return it.
await :: Useless State -> Server Message
await state = do
  sid <- getSessionID
  (clients, _) <- mkUseful state
  liftIO $ readIORef clients >>= maybe (return ("","")) C.takeMVar . lookup sid

-- | Scroll to the bottom of a textarea.
scrollToBottom :: Elem -> Client ()
scrollToBottom el = getProp el "scrollHeight" >>= setProp el "scrollTop"

-- | Client entry point.
clientMain :: API -> Client ()
clientMain api = withElems ["name","message","chat"] $ \[name, msg, chat] -> do
  -- Tell the server we're here, and fill out our backlog.
  -- The backlog is stored with newest messags first, so we need to reverse it.
  backlog <- map (\(n, m) -> n ++ ": " ++ m) <$> onServer (apiHello api)

  -- Ask the server for a new message, block until one arrives, repeat
  fork $ let awaitLoop chatlines = do
               setProp chat "value" . unlines . reverse $ take 100 chatlines
               scrollToBottom chat
               (from, msg) <- onServer $ apiAwait api
               awaitLoop $ (from ++ ": " ++ msg) : chatlines
         in awaitLoop backlog

  -- Send a message if the user hits return (charcode 13)
  msg `onEvent` OnKeyDown $ \k -> do
    case k of
      13 -> do
        m <- getProp msg "value"
        n <- getProp name "value"
        setProp msg "value" ""
        onServer $ apiSend api <.> (n :: String) <.> (m :: String)
      _ -> do
        return ()
  return ()

-- | Launch the application!
main :: IO ()
main = do
  -- Run the Haste.App application. Please note that a computation in the App
  -- monad should never contain any free variables.
  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    -- Create our state-holding elements
    state <- liftServerIO $ do
      clients <- newIORef []
      messages <- newIORef []
      return (clients, messages)

    -- Create an API object holding all available functions
    api <- API <$> export (hello state)
               <*> export (send state)
               <*> export (await state)

    -- Launch the client
    runClient $ clientMain api
