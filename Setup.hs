import Distribution.Simple
import System.Exit (ExitCode (..))
import System.Process (waitForProcess, runProcess)
import System.Directory (getAppUserDataDirectory)

main = defaultMainWithHooks $ simpleUserHooks {postInst = buildHasteStdLib}

-- The Haste standard library needs to be built so users can link to it.
buildHasteStdLib _ _ _ _ = do
  putStrLn "Building Haste standard library..."
  cabalDir <- getAppUserDataDirectory "cabal"
  let localHastec = cabalDir ++ "/bin/hastec"
  buildWith ["hastec", localHastec]

buildWith (hastec:hs) = do
  putStrLn $ "Attempting build with " ++ hastec
  build <- runProcess hastec
                      ["-O2", "--libinstall", "Haste", "Haste.Reactive"]
                      (Just "./src")
                      Nothing
                      Nothing
                      Nothing
                      Nothing
  res <- waitForProcess build
  case res of
    ExitFailure _ -> do
      putStrLn $ hastec ++ " couldn't build standard library,"
                        ++ " retrying with next compiler..."
      buildWith hs
    _ ->
      putStrLn "Standard library built successfully!"
buildWith _ = do
  fail $ "No compiler found that could build the standard library! " ++
         "This shouldn't happen!"
