{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 709
import qualified HastePkg708 as Real
#else
import qualified HastePkg710 as Real
#endif

main :: IO ()
main = Real.main
