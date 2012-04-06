{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, 
             FlexibleInstances, FlexibleContexts #-}
-- | Facilities for creating sinks for signals. A sink is something that
--   performs a side effect based on the input it receives from a signal.
module Haste.Reactive.Sink (Sink (..), sink) where
import Haste.Reactive.Signal
import Control.Applicative

-- | Turn any type into a sink. See the instance in Haste.Reactive.DOM for an
--   example.
class Sink s a | s -> a where
  (<<) :: s -> (Signal a) -> IO ()
infixl 0 <<

instance Sink (Pipe a) a where
  p << sig = sink (\s -> push s p) sig

class Snk a where
  type S a
  snk :: a -> S a

instance Snk (Signal (IO ())) where
  type S (Signal (IO ())) = IO ()
  snk = start . perform

instance (Snk (Signal b)) => Snk (Signal (a -> b)) where
  type S (Signal (a -> b)) = Signal a -> S (Signal b)
  snk f = \x -> snk $ f <*> x

-- | Create and start a signal from a variadic IO computation. This is quite
--   handy when creating concrete, not very complicated signals. The following
--   example will create a signal that responds to input by printing the input
--   backwards.
-- @
--   main = do
--     (p, sig) <- pipe ""
--     sink (putStrLn . reverse) sig
--     push "Hi!" p
--     push "OMG! It's sdrawkcab!" p
-- @
sink :: Snk (Signal b) => (a -> b) -> Signal a -> S (Signal b)
sink f x = snk (f <$> x)
