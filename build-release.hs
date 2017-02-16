{-# LANGUAGE CPP #-}
import Control.Shell
import Data.Bits
import qualified System.Info as Info (arch, os)
import System.Environment (getArgs)
import System.Exit

inBuildDir :: [String] -> Shell a -> Shell a
inBuildDir args act = do
  srcdir <- pwd
  isdir <- isDirectory "_build"
  when (isdir && not ("no-rebuild" `elem` args)) $ rmdir "_build"
  mkdir True "_build"
  inDirectory "_build" $ do
    unless ("no-rebuild" `elem` args) $ run "git" ["clone", srcdir]
    inDirectory "haste-compiler" act

-- Packages will end up in ghc-$GHC_MAJOR.$GHC_MINOR. If the directory does
-- not exist, it is created. If the package already exists in that directory,
-- it is overwritten.
main = shell_ $ do
    let args = fixAllArg cmdline
    when (null args) $ do
      echo $ "Usage: runghc build-release.hs [no-rebuild|in-place] formats\n"
      echo $ "Supported formats: deb, tarball, 7z, all\n"
      echo $ "no-rebuild\n  Repackage whatever is already in the " ++
             "_build directory\n  instead of rebuilding from scratch."
      echo $ "in-place\n  Build package in current directory.\n" ++
             "  Packages end up in ghc-$GHC_MAJOR.$GHC_MINOR."
      exit

    when ("--debghcdeps" `elem` args) $ do
      echo "ghc"
      exit

    let inplace = "in-place" `elem` args
        chdir = if inplace then id else inBuildDir args

    chdir $ do
      (ver, ghcver) <- if ("no-rebuild" `elem` args)
                         then do
                           getVersions
                         else do
                           vers <- buildPortable
                           bootPortable
                           return vers

      let (major, '.':rest) = break (== '.') ghcver
          (minor, _) = break (== '.') rest
          outdir
            | inplace   = "ghc-" ++ major ++ "." ++ minor
            | otherwise = ".." </> ".." </> ("ghc-" ++ major ++ "." ++ minor)
      mkdir True outdir

      when ("tarball" `elem` args) $ do
        tar <- buildBinaryTarball ver ghcver
        mv tar (outdir </> tar)

      when ("7z" `elem` args) $ do
        f <- buildBinary7z ver ghcver
        mv f (outdir </> f)

      when ("deb" `elem` args) $ do
        deb <- buildDebianPackage ver ghcver
        mv (".." </> deb) (outdir </> deb)
  where
    fixAllArg args | "all" `elem` args = "deb" : "tarball" : "7z" : args
                   | otherwise         = args

buildPortable = do
    -- Build compiler
    run "cabal" ["configure", "-f", "portable", "-f", "static"]
    inDirectory ("libraries/haste-lib") $ run "cabal" ["haddock"]
    run "dist/setup/setup" ["build"]

    -- Copy docs and build manpages
    cpdir "libraries/haste-lib/dist/doc/html/haste-lib" "haste-compiler/docs"
    buildManPages

    -- Strip symbols
    case Info.os of
      "mingw32" -> do
        -- windows
        run "strip" ["-s", "haste-compiler\\bin\\haste-pkg.exe"]
        run "strip" ["-s", "haste-compiler\\bin\\hastec.exe"]
        run "strip" ["-s", "haste-compiler\\bin\\haste-cat.exe"]
      "linux" -> do
        -- linux
        run "strip" ["-s", "haste-compiler/bin/haste-pkg"]
        run "strip" ["-s", "haste-compiler/bin/hastec"]
        run "strip" ["-s", "haste-compiler/bin/haste-cat"]
      _ -> do
        -- darwin
        run "strip" ["haste-compiler/bin/haste-pkg"]
        run "strip" ["haste-compiler/bin/hastec"]
        run "strip" ["haste-compiler/bin/haste-cat"]

    -- Get versions
    getVersions

getVersions = do
    ver <- fmap init $ capture $ run "haste-compiler/bin/hastec" ["--version"]
    ghcver <- fmap init $ capture $ run "ghc" ["--numeric-version"]
    return (ver, ghcver)

bootPortable = do
    -- Build libs
    run "haste-compiler/bin/haste-boot" ["--force", "--initial"]

    -- Remove unnecessary binaries
    case Info.os of
      "mingw32" -> do
        -- windows
        rm "haste-compiler\\bin\\haste-boot.exe"
      _ -> do
        -- linux/darwin
        rm "haste-compiler/bin/haste-boot"
    forEachFile "haste-compiler" $ \f -> do
      when ((f `hasExt` ".o") || (f `hasExt` ".a")) $ rm f
  where
    f `hasExt` e = takeExtension f == e

buildManPages = do
    mkdir True "man"
    buildManPage "hastec"
    buildManPage "haste-cat"
  where
    buildManPage inf = run "pandoc" ["-s", "-o", outf, inf']
      where
        ext | Info.os == "mingw32" = "html"
            | otherwise            = "1"
        outf = "man" </> inf <.> ext
        inf' = "doc" </> inf <.> "1.md"

buildBinaryTarball ver ghcver = do
    -- Copy manpages
    mkdir True "haste-compiler/man"
    cp "man/hastec.1" "haste-compiler/man/hastec.1"
    cp "man/haste-cat.1" "haste-compiler/man/haste-cat.1"

    -- Get versions and create binary tarball
    cp "install.sh" "haste-compiler/install.sh"
    cp "uninstall.sh" "haste-compiler/uninstall.sh"
    cp "doc/readme-portable-linux.txt" "haste-compiler/readme.txt"
    run "tar" ["-cjf", tarball, "haste-compiler"]
    mapM_ rm ["haste-compiler/install.sh",
              "haste-compiler/uninstall.sh",
              "haste-compiler/readme.txt"]
    rmdir "haste-compiler/man"
    return tarball
  where
    tarball =
      concat ["haste-compiler-",ver,"_ghc-",ghcver,"-",Info.os,".tar.bz2"]

buildBinary7z ver ghcver = do
    -- Copy HTML "manpages"
    mkdir True "haste-compiler/man"
    cp "man/hastec.html" "haste-compiler/man/hastec.html"
    cp "man/haste-cat.html" "haste-compiler/man/haste-cat.html"

    -- Get versions and create binary tarball
    run "7z" ["a", "-i!haste-compiler", name]
    return $ name
  where
    name =
      concat ["haste-compiler-",ver,"_ghc-",ghcver,"-",Info.os,".7z"]

deb_arch :: String
deb_arch
  | Info.arch == "x86_64" = "amd64" -- only amd64 supported
  | otherwise             = "i386"  -- not really supported, but may work

-- Debian packaging based on https://wiki.debian.org/IntroDebianPackaging.
-- Requires build-essential, devscripts and debhelper.
buildDebianPackage ver ghcver = do
  run "debuild" ["-e", "LD_LIBRARY_PATH=haste-compiler/haste-cabal",
                 "-us", "-uc", "-b"]
  return $ "haste-compiler_" ++ ver ++ "_" ++ deb_arch ++ ".deb"
