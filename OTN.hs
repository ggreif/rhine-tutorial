{-# LANGUAGE Arrows, DataKinds, RecordWildCards #-}


module OTN where

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond


-- | OTN stands for Optical Transport Network.
-- | Here we define seveal components (like NIMs,
-- | i.e non-intrusive monitors, terminations,
-- | cross-connection functions) according to the
-- | ITU standard (https://www.itu.int/rec/T-REC-G.709/en).

-- | Simple-minded OTU, only containing a higher-order ODU
--   note that we have no
--   - sizing (bitrate: OTU1..4)
--   - metadata
--   yet.

data OTU = OTU { ho :: ODU } deriving Show

-- | Simple-minded ODU, only carrying a payload and some metadata

data ODU = ODU { payload :: Int, sapi :: String, dapi :: String } deriving Show


-- pull a clock out of nothing...
type FrameClock = Millisecond 100

monitor :: SyncSF IO FrameClock ODU ODU
monitor = proc i@ODU{..} -> returnA -< i
