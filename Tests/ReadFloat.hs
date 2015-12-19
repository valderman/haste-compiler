module Tests.ReadFloat where

runTest
  = putStrLn
  $ show
  $ [ read (show n) :: Double
    | n <- [1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2]
    ]
