-- | Concurrency for Haste. Includes MVars, forking, Ajax and more.
module Haste.Concurrent (module Monad, module Ajax) where
import Haste.Concurrent.Monad as Monad
import Haste.Concurrent.Ajax as Ajax
