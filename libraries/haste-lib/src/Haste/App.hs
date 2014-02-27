-- | Type-safe client-server communication framework for Haste.
--   This module re-exports most of the Haste module for convenience since some
--   functions are exported by both Haste and Haste.App with different
--   definitions. For simplicity, import one or the other, not both.
module Haste.App (
    MonadIO, Serialize, Exportable, App, Server, Useless, Export, Done,
    Sessions, SessionID,
    liftServerIO, forkServerIO, export, runApp,
    (<.>), mkUseful, getSessionID, getActiveSessions, onSessionEnd,
    AppCfg, cfgURL, cfgPort, mkConfig,
    Client,
    runClient, onServer, liftIO,
    JSString, JSAny, URL, alert, prompt, eval, writeLog, catJSStr, fromJSStr,
    module Haste.App.Events,
    module Haste.DOM,
    module Haste.Random,
    module Haste.JSType,
    module Haste.Hash
  ) where
import Haste.App.Client
import Haste.App.Monad
import Haste.App.Events
import Haste.Serialize
import Haste.DOM
import Haste.Random
import Haste.JSType
import Haste.Hash
import Haste
import Control.Monad.IO.Class
