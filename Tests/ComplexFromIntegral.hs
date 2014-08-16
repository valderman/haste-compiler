module Tests.ComplexFromIntegral where
import Data.Complex

runTest = do
  putStrLn $ show $ p' where
    p' = zipWith (*) (map (fromIntegral) [1..3]) p
    p = [1 :+ 0, 2 :+ 0, 3 :+ 0]
