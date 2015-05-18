{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, PatternGuards, ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Handle.FD
-- Copyright   :  (c) The University of Glasgow, 1994-2008
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Handle operations implemented by file descriptors (FDs)
--
-----------------------------------------------------------------------------

module GHC.IO.Handle.FD ( 
  stdin, stdout, stderr,
  openFile, openBinaryFile, openFileBlocking,
  mkHandleFromFD, fdToHandle, fdToHandle',
  isEOF
 ) where

import GHC.Base
import GHC.Show
import Data.Maybe
import Foreign.C.Types
import GHC.MVar
import GHC.IO
import GHC.IO.Encoding
import GHC.IO.Device as IODevice
import GHC.IO.Exception
import GHC.IO.IOMode
import GHC.IO.Handle
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import qualified GHC.IO.FD as FD
import qualified System.Posix.Internals as Posix
import qualified Haste.Handle

-- ---------------------------------------------------------------------------
-- Standard Handles

-- Three handles are allocated during program initialisation.  The first
-- two manage input or output from the Haskell program's standard input
-- or output channel respectively.  The third manages output to the
-- standard error channel. These handles are initially open.

-- | A handle managing input from the Haskell program's standard input channel.
stdin :: Handle
stdin = Haste.Handle.stdin

-- | A handle managing output to the Haskell program's standard output channel.
stdout :: Handle
stdout = Haste.Handle.stdout

-- | A handle managing output to the Haskell program's standard error channel.
stderr :: Handle
stderr = Haste.Handle.stderr

stdHandleFinalizer :: FilePath -> MVar Handle__ -> IO ()
stdHandleFinalizer fp m = do
  h_ <- takeMVar m
  flushWriteBuffer h_
  case haType h_ of 
      ClosedHandle -> return ()
      _other       -> closeTextCodecs h_
  putMVar m (ioe_finalizedHandle fp)

-- We have to put the FDs into binary mode on Windows to avoid the newline
-- translation that the CRT IO library does.
setBinaryMode :: FD.FD -> IO ()
#ifdef mingw32_HOST_OS
setBinaryMode fd = do _ <- setmode (FD.fdFD fd) True
                      return ()
#else
setBinaryMode _ = return ()
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "__hscore_setmode"
  setmode :: CInt -> Bool -> IO CInt
#endif

-- ---------------------------------------------------------------------------
-- isEOF

-- | The computation 'isEOF' is identical to 'hIsEOF',
-- except that it works only on 'stdin'.

isEOF :: IO Bool
isEOF = hIsEOF stdin

-- ---------------------------------------------------------------------------
-- Opening and Closing Files

addFilePathToIOError :: String -> FilePath -> IOException -> IOException
addFilePathToIOError fun fp ioe
  = ioe{ ioe_location = fun, ioe_filename = Just fp }

-- | Computation 'openFile' @file mode@ allocates and returns a new, open
-- handle to manage the file @file@.  It manages input if @mode@
-- is 'ReadMode', output if @mode@ is 'WriteMode' or 'AppendMode',
-- and both input and output if mode is 'ReadWriteMode'.
--
-- If the file does not exist and it is opened for output, it should be
-- created as a new file.  If @mode@ is 'WriteMode' and the file
-- already exists, then it should be truncated to zero length.
-- Some operating systems delete empty files, so there is no guarantee
-- that the file will exist following an 'openFile' with @mode@
-- 'WriteMode' unless it is subsequently written to successfully.
-- The handle is positioned at the end of the file if @mode@ is
-- 'AppendMode', and otherwise at the beginning (in which case its
-- internal position is 0).
-- The initial buffer mode is implementation-dependent.
--
-- This operation may fail with:
--
--  * 'isAlreadyInUseError' if the file is already open and cannot be reopened;
--
--  * 'isDoesNotExistError' if the file does not exist; or
--
--  * 'isPermissionError' if the user does not have permission to open the file.
--
-- Note: if you will be working with files containing binary data, you'll want to
-- be using 'openBinaryFile'.
openFile :: FilePath -> IOMode -> IO Handle
openFile fp im = error "No file IO! Sorry!"
--  catchException
--    (openFile' fp im dEFAULT_OPEN_IN_BINARY_MODE True)
--    (\e -> ioError (addFilePathToIOError "openFile" fp e))

-- | Like 'openFile', but opens the file in ordinary blocking mode.
-- This can be useful for opening a FIFO for reading: if we open in
-- non-blocking mode then the open will fail if there are no writers,
-- whereas a blocking open will block until a writer appears.
openFileBlocking :: FilePath -> IOMode -> IO Handle
openFileBlocking fp im = error "No file IO! Sorry!"
--  catchException
--    (openFile' fp im dEFAULT_OPEN_IN_BINARY_MODE False)
--    (\e -> ioError (addFilePathToIOError "openFile" fp e))

-- | Like 'openFile', but open the file in binary mode.
-- On Windows, reading a file in text mode (which is the default)
-- will translate CRLF to LF, and writing will translate LF to CRLF.
-- This is usually what you want with text files.  With binary files
-- this is undesirable; also, as usual under Microsoft operating systems,
-- text mode treats control-Z as EOF.  Binary mode turns off all special
-- treatment of end-of-line and end-of-file characters.
-- (See also 'hSetBinaryMode'.)

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile fp m = error "No file IO! Sorry!"
--  catchException
--    (openFile' fp m True True)
--    (\e -> ioError (addFilePathToIOError "openBinaryFile" fp e))


-- ---------------------------------------------------------------------------
-- Converting file descriptors to Handles

-- ---------------------------------------------------------------------------
-- Are files opened by default in text or binary mode, if the user doesn't
-- specify?

dEFAULT_OPEN_IN_BINARY_MODE :: Bool
dEFAULT_OPEN_IN_BINARY_MODE = False

mkHandleFromFD :: FD.FD -> IODeviceType -> FilePath -> IOMode -> Bool -> Maybe TextEncoding -> IO Handle
mkHandleFromFD = error "mkHandleFromFD not implemented"

fdToHandle' :: CInt -> Maybe IODeviceType -> Bool -> FilePath -> IOMode -> Bool -> IO Handle
fdToHandle' = error "fdToHandle' not implemented"

fdToHandle :: Posix.FD -> IO Handle
fdToHandle = error "fdToHandle not implemented"
