{-# LANGUAGE Arrows, DataKinds, RecordWildCards, BangPatterns #-}

module OTN where

-- * Imports

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond
import Data.IORef
import System.IO.Unsafe

-- * Data types

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

-- * Various functional model elements

-- | monitor - check whether the metadata is as expected
monitor :: SyncSF IO FrameClock ODU ODU
monitor = proc i@ODU{..} -> do
  arrMSync putStrLn -< "incsapi: " ++ sapi
  returnA -< i

-- | terminate - peel out a higher-order ODU from an OTU
terminate :: SyncSF IO FrameClock OTU ODU
terminate = proc OTU{..} -> returnA -< ho

-- | assemble - put together an OTU given a higher-order ODU
assemble :: SyncSF IO FrameClock ODU OTU
assemble = proc ho -> returnA -< OTU{..}

-- ** Ports

-- | a rudimentary input port (we could model a fiber too!)
portIn :: SyncSignal IO FrameClock OTU
portIn = proc _ -> returnA -< OTU{ ho = ODU{payload=42, sapi = "Berlin", dapi = "Köln"}}

-- | a rudimentary output port
portOut :: SyncSF IO FrameClock OTU ()
portOut = proc _ -> returnA -< ()

-- ** Crossconnect

-- TBD

{-# NOINLINE xcConf #-}
xcConf :: IORef (Int -> Maybe Int) -- output-oriented matrix
!xcConf = unsafePerformIO $ newIORef $ const Nothing

cc :: SyncSF IO FrameClock (ODU, ODU) (ODU, ODU)
cc = undefined


-- * Tests

-- | Build a pipeline and provide a clock. Run the whole thing in IO.
otnTest = flow $ pipeline @@ waitClock
  where pipeline = portIn >-> terminate >-> monitor
               >-> frameCount
               >-> assemble >-> portOut

-- * Utilities

frameCount :: SyncSF IO FrameClock a a
frameCount = syncId &&& count >-> arrMSync print >>> arr fst

-- * TODOs
-- - model crossconnect function
-- - model AIS (OTU/ODU)
-- - model lower-order ODUs
-- - model bandwidths
-- - model substructuring
-- - model timing domains (IF/XC)
-- - model phase jumps
-- - model framers
-- - model all-zeros (XC)
-- - model RDI
-- - model OCI
-- - model defects / alarms / faults
-- - model correlation F4, etc.
-- - model loopbacks
-- - model quality (biterrors, ES, SES)
