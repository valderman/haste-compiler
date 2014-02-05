-- | Type-safe client-server communication for Haste.
module XHaste (
  Exportable, Server, Useless, Export, Done,
  liftIO, export, mkUseful, runServer, (<.>),
  Client,
  runClient, onServer) where
import XHaste.Client
import XHaste.Server
