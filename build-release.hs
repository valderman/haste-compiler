{-# LANGUAGE CPP #-}
import Control.Shell
import Data.Bits
import System.Info
import Control.Monad
import System.Environment (getArgs)

-- Pass 'deb' to build a deb package, 'tarball' to build a tarball, 'all' to
-- build all supported formats, and 'no-rebuild' to avoid rebuilding stuff,
-- only re-packaging it.
main = do
    args <- fixAllArg `fmap` getArgs
    res <- shell $ do
      srcdir <- pwd
      isdir <- isDirectory "_build"
      when (isdir && not ("no-rebuild" `elem` args)) $ rmdir "_build"
      mkdir True "_build"

      inDirectory "_build" $ do
        unless ("no-rebuild" `elem` args) $ do
          run_ "git" ["clone", srcdir] ""
        inDirectory "haste-compiler" $ do
          (ver, ghcver) <- if ("no-rebuild" `elem` args)
                             then do
                               getVersions
                             else do
                               vers <- buildPortable
                               bootPortable
                               return vers

          when ("tarball" `elem` args) $ do
            tar <- buildBinaryTarball ver ghcver
            mv tar (".." </> tar)

          when ("deb" `elem` args) $ do
            buildDebianPackage srcdir ver ghcver
    case res of
      Left err -> error $ "FAILED: " ++ err
      _        -> return ()
  where
    fixAllArg args | "all" `elem` args = "deb" : "tarball" : args
                   | otherwise         = args

buildSourceTarball srcdir ver = do
    run_ "git" ["clone", srcdir, "haste-compiler-" ++ ver] ""
    run_ "tar" ["-cjf", srctar, "haste-compiler-" ++ ver] ""
    return srctar
  where
    srctar = "haste-compiler_" ++ ver ++ ".orig.tar.bz2"

buildPortable = do
    -- Build compiler
    run_ "cabal" ["configure", "-f", "portable"] ""
    run_ "cabal" ["build"] ""

    -- Strip symbols
    run_ "strip" ["-s", "haste-compiler/bin/haste-boot"] ""
    run_ "strip" ["-s", "haste-compiler/bin/haste-pkg"] ""
    run_ "strip" ["-s", "haste-compiler/bin/haste-inst"] ""
    run_ "strip" ["-s", "haste-compiler/bin/hastec"] ""

    -- Get versions
    getVersions

getVersions = do
    ver <- fmap init $ run "haste-compiler/bin/hastec" ["--version"] ""
    ghcver <- fmap init $ run "ghc" ["--numeric-version"] ""
    return (ver, ghcver)

bootPortable = do
    -- Build libs
    run_ "haste-compiler/bin/haste-boot" ["--force", "--local"] ""

    -- Remove unnecessary binaries
    rm "haste-compiler/bin/haste-copy-pkg"
    rm "haste-compiler/bin/haste-install-his"
    rm "haste-compiler/bin/haste-boot"
    run_ "find" ["haste-compiler", "-name", "*.o", "-exec", "rm", "{}", ";"] ""
    run_ "find" ["haste-compiler", "-name", "*.a", "-exec", "rm", "{}", ";"] ""

buildBinaryTarball ver ghcver = do
    -- Get versions and create binary tarball
    run_ "tar" ["-cjf", tarball, "haste-compiler"] ""
    return tarball
  where
    tarball =
      concat ["haste-compiler-",ver,"-ghc-",ghcver,"-",os,"-",arch,".tar.bz2"]
    arch = if bits == 64 then "x86_64" else "i686"
#if __GLASGOW_HASKELL__ >= 708
    bits = finiteBitSize (0 :: Int)
#else
    bits = bitSize (0 :: Int)
#endif

-- Debian packaging based on https://wiki.debian.org/IntroDebianPackaging.
-- Requires build-essential, devscripts and debhelper.
buildDebianPackage srcdir ver ghcver = do
    _ <- buildSourceTarball srcdir ver
    run_ "debuild" ["-us", "-uc", "-b"] ""
