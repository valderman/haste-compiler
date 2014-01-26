{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ForeignFunctionInterface, CApiFFI,
             EmptyDataDecls #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Internals
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires POSIX)
--
-- POSIX support layer for the standard libraries.
-- This library is built on *every* platform, including Win32.
--
-- Non-posix compliant in order to support the following features:
--      * S_ISSOCK (no sockets in POSIX)
--
-----------------------------------------------------------------------------

-- #hide
module System.Posix.Internals where

#ifdef __NHC__
#define HTYPE_TCFLAG_T
#else
# include "HsBaseConfig.h"
#endif

#if ! (defined(mingw32_HOST_OS) || defined(__MINGW32__))
import Control.Monad
#endif
import System.Posix.Types

import Foreign
import Foreign.C

-- import Data.Bits
import Data.Maybe

#if !defined(HTYPE_TCFLAG_T)
import System.IO.Error
#endif

#if __GLASGOW_HASKELL__
import GHC.Base
import GHC.Num
import GHC.Real
import GHC.IO
import GHC.IO.IOMode
import GHC.IO.Exception
import GHC.IO.Device
#ifndef mingw32_HOST_OS
import {-# SOURCE #-} GHC.IO.Encoding (getFileSystemEncoding)
import qualified GHC.Foreign as GHC
#endif
#elif __HUGS__
import Hugs.Prelude (IOException(..), IOErrorType(..))
import Hugs.IO (IOMode(..))
#elif __NHC__
import GHC.IO.Device	-- yes, I know, but its portable, really!
import System.IO
import Control.Exception
import DIOError
#endif

#ifdef __HUGS__
{-# CFILES cbits/PrelIOUtils.c cbits/consUtils.c #-}
#endif


-- ---------------------------------------------------------------------------
-- Debugging the base package

puts :: String -> IO ()
puts s = withCAStringLen (s ++ "\n") $ \(p, len) -> do
            -- In reality should be withCString, but assume ASCII to avoid loop
            -- if this is called by GHC.Foreign
           _ <- c_write 1 (castPtr p) (fromIntegral len)
           return ()


-- ---------------------------------------------------------------------------
-- Types

type CFLock     = ()
type CGroup     = ()
type CLconv     = ()
type CPasswd    = ()
type CSigaction = ()
data {-# CTYPE "sigset_t" #-} CSigset
type CStat      = ()
type CTermios   = ()
type CTm        = ()
type CTms       = ()
type CUtimbuf   = ()
type CUtsname   = ()

type FD = CInt

-- ---------------------------------------------------------------------------
-- stat()-related stuff

fdFileSize :: FD -> IO Integer
fdFileSize _ = error "fdFileSize is not implemented"

fileType :: FilePath -> IO IODeviceType
fileType _ = error "fileType is not implemented"

-- NOTE: On Win32 platforms, this will only work with file descriptors
-- referring to file handles. i.e., it'll fail for socket FDs.
fdStat :: FD -> IO (IODeviceType, CDev, CIno)
fdStat _ = error "fdStat is not implemented"
    
fdType :: FD -> IO IODeviceType
fdType _ = error "fdType is not implemented"

statGetType :: Ptr CStat -> IO IODeviceType
statGetType _ = error "statGetType is not implemented"
    
ioe_unknownfiletype :: IOException
#ifndef __NHC__
ioe_unknownfiletype = IOError Nothing UnsupportedOperation "fdType"
                        "unknown file type"
#  if __GLASGOW_HASKELL__
                        Nothing
#  endif
                        Nothing
#else
ioe_unknownfiletype = UserError "fdType" "unknown file type"
#endif

fdGetMode :: FD -> IO IOMode
fdGetMode _ = error "fdGetMode is not implemented"

#ifdef mingw32_HOST_OS
withFilePath :: FilePath -> (CWString -> IO a) -> IO a
peekFilePath :: CWString -> IO FilePath
#else

withFilePath :: FilePath -> (CString -> IO a) -> IO a
peekFilePath :: CString -> IO FilePath
peekFilePathLen :: CStringLen -> IO FilePath

withFilePath = withCString
peekFilePath = peekCString
peekFilePathLen = peekCStringLen

#endif

-- ---------------------------------------------------------------------------
-- Terminal-related stuff

setEcho :: FD -> Bool -> IO ()
setEcho _ _ = return () -- always false for browsers

getEcho :: FD -> IO Bool
getEcho _ = return False -- always false for browsers

setCooked :: FD -> Bool -> IO ()
setCooked _ _ = return ()

tcSetAttr :: FD -> (Ptr CTermios -> IO a) -> IO a
tcSetAttr _ _ = error "tcSetAttr not implemented"

get_saved_termios :: CInt -> IO (Ptr CTermios)
get_saved_termios _ = error "get_saved_termios not implemented"

set_saved_termios :: CInt -> (Ptr CTermios) -> IO ()
set_saved_termios _ _ = error "set_saved_termios not implemented"

-- ---------------------------------------------------------------------------
-- Turning on non-blocking for a file descriptor

setNonBlockingFD :: FD -> Bool -> IO ()
setNonBlockingFD _ _ = return () -- doesn't do anything in a browser

-- -----------------------------------------------------------------------------
-- Set close-on-exec for a file descriptor

setCloseOnExec :: FD -> IO ()
setCloseOnExec fd = return () -- does nothing for a browser

-- -----------------------------------------------------------------------------
-- foreign imports

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
type CFilePath = CString
#else
type CFilePath = CWString
#endif

c_access :: CString -> CInt -> IO CInt
c_access = error "c_access not implemented"

c_chmod :: CString -> CMode -> IO CInt
c_chmod = error "c_chmod not implemented"

c_close :: CInt -> IO CInt
c_close = error "c_close not implemented"

c_creat :: CString -> CMode -> IO CInt
c_creat = error "c_creat not implemented"

c_dup :: CInt -> IO CInt
c_dup = error "c_dup not implemented"

c_dup2 :: CInt -> CInt -> IO CInt
c_dup2 = error "c_dup2 not implemented"

c_fstat :: CInt -> Ptr CStat -> IO CInt
c_fstat = error "c_fstat not implemented"

c_isatty :: CInt -> IO CInt
c_isatty = error "c_isatty not implemented"


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
c_lseek :: CInt -> Int64 -> CInt -> IO Int64
c_lseek = error "c_lseek not implemented"
#else
-- We use CAPI as on some OSs (eg. Linux) this is wrapped by a macro
-- which redirects to the 64-bit-off_t versions when large file
-- support is enabled.
c_lseek :: CInt -> COff -> CInt -> IO COff
c_lseek = error "c_lseek not implemented"
#endif

lstat :: CFilePath -> Ptr CStat -> IO CInt
lstat = error "lstat not implemented"

c_open :: CFilePath -> CInt -> CMode -> IO CInt
c_open = error "c_open not implemented"

c_safe_open :: CFilePath -> CInt -> CMode -> IO CInt
c_safe_open = error "c_safe_open not implemented"

c_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize
c_read = error "c_read not implemented"

c_safe_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize
c_safe_read = error "c_safe_read not implemented"

c_stat :: CFilePath -> Ptr CStat -> IO CInt
c_stat = error "c_stat not implemented"

c_umask :: CMode -> IO CMode
c_umask = error "c_umask not implemented"

c_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
c_write = error "c_write not implemented"

c_safe_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
c_safe_write = error "c_safe_write not implemented"

c_ftruncate :: CInt -> COff -> IO CInt
c_ftruncate = error "c_ftruncate not implemented"

c_unlink :: CString -> IO CInt
c_unlink = error "c_unlink not implemented"

c_getpid :: IO CPid
c_getpid = return 0

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
c_fcntl_read  :: CInt -> CInt -> IO CInt
c_fcntl_read = error "c_fcntl_read not implemented"

c_fcntl_write :: CInt -> CInt -> CLong -> IO CInt
c_fcntl_write = error "c_fcntl_write not implemented"

c_fcntl_lock  :: CInt -> CInt -> Ptr CFLock -> IO CInt
c_fcntl_lock = error "c_fcntl_lock not implemented"

c_fork :: IO CPid
c_fork = error "c_fork not implemented"

c_link :: CString -> CString -> IO CInt
c_link = error "c_link not implemented"

c_mkfifo :: CString -> CMode -> IO CInt
c_mkfifo = error "c_mkfifo not implemented"

c_pipe :: Ptr CInt -> IO CInt
c_pipe = error "c_pipe not implemented"

c_sigemptyset :: Ptr CSigset -> IO CInt
c_sigemptyset = error "c_sigemptyset not implemented"

c_sigaddset :: Ptr CSigset -> CInt -> IO CInt
c_sigaddset = error "c_sigaddset not implemented"

c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO CInt
c_sigprocmask = error "c_sigprocmask not implemented"

c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt
c_tcgetattr = error "c_tcgetattr not implemented"

c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt
c_tcsetattr = error "c_tcsetattr not implemented"

c_utime :: CString -> Ptr CUtimbuf -> IO CInt
c_utime = error "c_utime not implemented"

c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
c_waitpid = error "c_waitpid not implemented"
#endif

-- POSIX flags only:
foreign import ccall unsafe "HsBase.h __hscore_o_rdonly" o_RDONLY :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_wronly" o_WRONLY :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_rdwr"   o_RDWR   :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_append" o_APPEND :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_creat"  o_CREAT  :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_excl"   o_EXCL   :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_trunc"  o_TRUNC  :: CInt

-- non-POSIX flags.
foreign import ccall unsafe "HsBase.h __hscore_o_noctty"   o_NOCTTY   :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_nonblock" o_NONBLOCK :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_binary"   o_BINARY   :: CInt

foreign import capi unsafe "sys/stat.h S_ISREG"  c_s_isreg  :: CMode -> CInt
foreign import capi unsafe "sys/stat.h S_ISCHR"  c_s_ischr  :: CMode -> CInt
foreign import capi unsafe "sys/stat.h S_ISBLK"  c_s_isblk  :: CMode -> CInt
foreign import capi unsafe "sys/stat.h S_ISDIR"  c_s_isdir  :: CMode -> CInt
foreign import capi unsafe "sys/stat.h S_ISFIFO" c_s_isfifo :: CMode -> CInt

s_isreg  :: CMode -> Bool
s_isreg cm = c_s_isreg cm /= 0
s_ischr  :: CMode -> Bool
s_ischr cm = c_s_ischr cm /= 0
s_isblk  :: CMode -> Bool
s_isblk cm = c_s_isblk cm /= 0
s_isdir  :: CMode -> Bool
s_isdir cm = c_s_isdir cm /= 0
s_isfifo :: CMode -> Bool
s_isfifo cm = c_s_isfifo cm /= 0

foreign import ccall unsafe "HsBase.h __hscore_sizeof_stat" sizeof_stat :: Int
foreign import ccall unsafe "HsBase.h __hscore_st_mtime" st_mtime :: Ptr CStat -> IO CTime
#ifdef mingw32_HOST_OS
foreign import ccall unsafe "HsBase.h __hscore_st_size" st_size :: Ptr CStat -> IO Int64
#else
foreign import ccall unsafe "HsBase.h __hscore_st_size" st_size :: Ptr CStat -> IO COff
#endif
foreign import ccall unsafe "HsBase.h __hscore_st_mode" st_mode :: Ptr CStat -> IO CMode
foreign import ccall unsafe "HsBase.h __hscore_st_dev" st_dev :: Ptr CStat -> IO CDev
foreign import ccall unsafe "HsBase.h __hscore_st_ino" st_ino :: Ptr CStat -> IO CIno

foreign import ccall unsafe "HsBase.h __hscore_echo"         const_echo :: CInt
foreign import ccall unsafe "HsBase.h __hscore_tcsanow"      const_tcsanow :: CInt
foreign import ccall unsafe "HsBase.h __hscore_icanon"       const_icanon :: CInt
foreign import ccall unsafe "HsBase.h __hscore_vmin"         const_vmin   :: CInt
foreign import ccall unsafe "HsBase.h __hscore_vtime"        const_vtime  :: CInt
foreign import ccall unsafe "HsBase.h __hscore_sigttou"      const_sigttou :: CInt
foreign import ccall unsafe "HsBase.h __hscore_sig_block"    const_sig_block :: CInt
foreign import ccall unsafe "HsBase.h __hscore_sig_setmask"  const_sig_setmask :: CInt
foreign import ccall unsafe "HsBase.h __hscore_f_getfl"      const_f_getfl :: CInt
foreign import ccall unsafe "HsBase.h __hscore_f_setfl"      const_f_setfl :: CInt
foreign import ccall unsafe "HsBase.h __hscore_f_setfd"      const_f_setfd :: CInt
foreign import ccall unsafe "HsBase.h __hscore_fd_cloexec"   const_fd_cloexec :: CLong

#if defined(HTYPE_TCFLAG_T)
foreign import ccall unsafe "HsBase.h __hscore_sizeof_termios"  sizeof_termios :: Int
foreign import ccall unsafe "HsBase.h __hscore_sizeof_sigset_t" sizeof_sigset_t :: Int

foreign import ccall unsafe "HsBase.h __hscore_lflag" c_lflag :: Ptr CTermios -> IO CTcflag
foreign import ccall unsafe "HsBase.h __hscore_poke_lflag" poke_c_lflag :: Ptr CTermios -> CTcflag -> IO ()
foreign import ccall unsafe "HsBase.h __hscore_ptr_c_cc" ptr_c_cc  :: Ptr CTermios -> IO (Ptr Word8)
#endif

s_issock :: CMode -> Bool
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
s_issock cmode = c_s_issock cmode /= 0
foreign import capi unsafe "sys/stat.h S_ISSOCK" c_s_issock :: CMode -> CInt
#else
s_issock _ = False
#endif

foreign import ccall unsafe "__hscore_bufsiz"  dEFAULT_BUFFER_SIZE :: Int
sEEK_CUR, sEEK_SET, sEEK_END :: CInt
sEEK_CUR = 0
sEEK_SET = 1
sEEK_END = 2

{-
Note: CSsize

On Win64, ssize_t is 64 bit, but functions like read return 32 bit
ints. The CAPI wrapper means the C compiler takes care of doing all
the necessary casting.

When using ccall instead, when the functions failed with -1, we thought
they were returning with 4294967295, and so didn't throw an exception.
This lead to a segfault in echo001(ghci).
-}

