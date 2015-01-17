-- | Event handling for Haste.
module Haste.Events (
    module Core,
    module BasicEvents,
    module KeyEvents,
    module MouseEvents
  ) where
import Haste.Events.Core as Core
import Haste.Events.BasicEvents as BasicEvents
import Haste.Events.KeyEvents as KeyEvents
import Haste.Events.MouseEvents as MouseEvents
