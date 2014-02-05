-- | Type-safe client-server communication for Haste.
module XHaste (
  Exportable, Server, Useless, Export, Done,
  liftIO, export, mkUseful, runServer, (<.>),
  AppCfg, cfgURL, cfgPort, defaultConfig,
  Client,
  runClient, onServer, liftCIO) where
import XHaste.Client
import XHaste.Server
