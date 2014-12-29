-- | Type-safe client-server communication framework for Haste.
--
--   In addition to the Haste.App extras, this module exports the same API as
--   "Haste", modified slightly to work better with the automatic program
--   slicing Haste.App provides. This means that you should import either this
--   module *or* Haste, but *not* both.
module Haste.App (
    MonadIO, Remotable, App, Server, Remote, Done,
    Sessions, SessionID,
    liftServerIO, forkServerIO, remote, runApp,
    (<.>), getSessionID, getActiveSessions, onSessionEnd,
    AppCfg, cfgHost, cfgPort, mkConfig,
    Client,
    runClient, onServer, liftIO,
    JSString, JSAny, URL, alert, prompt, eval, writeLog, catJSStr, fromJSStr,
    module Haste.App.Events,
    module Haste.DOM.Core,
    module Haste.Random,
    module Haste.JSType,
    module Haste.Hash,
    module Haste.Binary,
    module Data.Default
  ) where
import Haste.App.Client
import Haste.App.Monad
import Haste.App.Events
import Haste.Binary (Binary (..))
import Haste.DOM.Core
import Haste.Random
import Haste.JSType
import Haste.Hash
import Haste
import Control.Monad.IO.Class
import Data.Default
