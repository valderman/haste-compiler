{-# LANGUAGE CPP #-}
import Control.Shell
import Data.Bits
import System.Info
import Control.Monad

main = do
  res <- shell $ do
    srcdir <- pwd
    isdir <- isDirectory "_build"
    when isdir $ rmdir "_build"
    mkdir True "_build"
    inDirectory "_build" $ do
      run_ "git" ["clone", srcdir] ""
      inDirectory "haste-compiler" $ do
        (ver, ghcver) <- buildPortable
        bootPortable
        tar <- buildBinaryTarball ver ghcver
        mv tar (".." </> tar)
        buildDebianPackage srcdir ver ghcver
  case res of
    Left err -> error $ "FAILED: " ++ err
    _        -> return ()

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
    return deb
  where
    deb = concat ["haste-compiler_", ver, "-1_amd64.deb"]
