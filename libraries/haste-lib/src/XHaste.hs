-- | Type-safe client-server communication for Haste.
module XHaste (
  Exportable, App, Useless, Export, Done,
  liftIO, export, mkUseful, runApp, (<.>),
  AppCfg, cfgURL, cfgPort, defaultConfig,
  Client,
  runClient, onServer, liftCIO) where
import XHaste.Client
import XHaste.Server
