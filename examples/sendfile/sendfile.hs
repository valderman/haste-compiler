-- | Simple example of storing a file on a server using Haste.App.
--   Please remember to never, ever, use unsanitized file names received from
--   a client on the server, as is done in this example!
import Haste.App
import Haste.Binary
import qualified Data.ByteString.Lazy as BS

main = runApp def $ do
  upload <- export $ \name file -> do
    filedata <- getBlobData file
    liftIO $ BS.writeFile name (toByteString filedata)

  runClient $ withElems ["file","upload"] $ \[file,btn] -> do
    btn `onEvent` OnClick $ \_ _ -> do
      mfd <- getFileData file 0
      case mfd of
        Just fd -> do
          fn <- getFileName file
          onServer $ upload <.> fn <.> fd
          alert "File uploaded!"
        _ -> do
          alert "You need to specify a file first."
