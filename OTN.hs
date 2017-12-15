{-# LANGUAGE Arrows, DataKinds, RecordWildCards, BangPatterns, ScopedTypeVariables, RankNTypes #-}

module OTN where

-- * Imports

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond
import Data.IORef
import System.IO.Unsafe
import Data.Functor.Identity

-- * Data types

-- | OTN stands for Optical Transport Network.
-- | Here we define several components (like NIMs,
-- | i.e non-intrusive monitors, terminations,
-- | cross-connection functions) according to the
-- | ITU standard (https://www.itu.int/rec/T-REC-G.709/en).

-- | Simple-minded OTU, only containing a higher-order ODU
--   note that we have no
--   - sizing (bitrate: OTU1..4)
--   - metadata
--   yet.

-- * defects

--adaptive information
data AI = AI { ai_CK, ai_D, ai_FS, ai_MFS, ai_TSF, ai_TSD :: Bool }
-- remote information
data RI = RI { ri_BDI, ri_BEI, ri_BIAE :: Bool }
-- characteristic information
data CI = CI { ci_CK, ci_D, ci_FS, ci_MFS, ci_SSF :: Bool }
-- management information
data MI = MI { mi_ExSAPI, mi_ExDAPI, mi_GetAcTI, mi_TIMDetMo, mi_TIMActDis, mi_DEGThr, mi_DEGM, mi_1second :: Bool }
-- fault causes
data Cause = Cause { mi_AcTI, mi_cTIM, mi_cDEG, mi_cBDI, mi_cSSF, mi_pN_EBC
                   , mi_pN_DS, mi_pF_EBC, mi_pF_DS, mi_pBIAE, mi_pIAE :: Bool }

{-
OTUk_TCP:
OTUk_CI_CK
OTUk_CI_D
OTUk_CI_FS
OTUk_CI_MFS
OTUk_CI_SSF

OTUk_TT_Sk_MP:
OTUk_TT_Sk_MI_ExSAPI
OTUk_TT_Sk_MI_ExDAPI
OTUk_TT_Sk_MI_GetAcTI
OTUk_TT_Sk_MI_TIMDetMo
OTUk_TT_Sk_MI_TIMActDis
OTUk_TT_Sk_MI_DEGThr
OTUk_TT_Sk_MI_DEGM
OTUk_TT_Sk_MI_1second
-}

-- raw defects, internal to OTU
data D = D { dTIM, dIAE, dDEG, dBDI :: Bool }

correlator :: MI -> CI -> D -> Cause
correlator MI{..} CI{..} D{..} = Cause{..}
  where -- anomalies
        aBDI = ci_SSF `or` dTIM
        aBEI = False -- nBIPV
        aBIAE = dIAE
        aTSF = ci_SSF `or` (dTIM `and` (not mi_TIMActDis))
        aTSD = dDEG
        -- fault causes
        mi_cTIM = dTIM `and` (not ci_SSF)
        mi_cDEG = dDEG `and` (not ci_SSF) `and` (not (dTIM `and` (not mi_TIMActDis)))
        mi_cBDI = dBDI `and` (not ci_SSF) `and` (not (dTIM `and` (not mi_TIMActDis)))
        mi_cSSF = ci_SSF
        or = (||)
        and = (&&)


data OTU = OTU { ho :: ODU } | OTUAIS deriving Show

-- | Simple-minded ODU, only carrying a payload and some metadata
-- | as well as maintenance signals
data ODU = ODU { payload :: Int, sapi :: String, dapi :: String } | AIS | OCI deriving Show


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
cc = proc (i1, i2) -> do
  conf <- arrMSync readIORef -< xcConf
  arrMSync print -< (conf 1, conf 2)
  let newConf 1 = Nothing
      newConf 2 = Just 1
  arrMSync (writeIORef xcConf) -< newConf
  returnA -< (i2, i1)

-- * Tests

-- | Build a pipeline and provide a clock. Run the whole thing in IO.
otnTest = flow $ pipeline @@ waitClock
  where pipeline = portIn >-> terminate >-> monitor
               >-> frameCount
               >-> arr (\x->(x,x)) >-> cc >-> arr snd
               >-> monitor >-> assemble >-> portOut

-- * Utilities

frameCount :: SyncSF IO FrameClock a a
frameCount = syncId &&& count >-> arrMSync print >>> arr fst


-- | Applies a function to the input and an accumulator, returning the
-- updated accumulator and output. FIXME: soon part of dunai
mealy :: Monad m => (a -> s -> (b, s)) -> s -> MSF m a b
mealy f s0 = feedback s0 $ arr $ uncurry f

-- | simulates a monad-polymorphic stream function
simulate :: (forall m. Monad m => MSF m a b) -> [a] -> [b]
simulate arr = runIdentity . embed arr


-- * Framing
framer :: Monad m => [Bool] -> Int {-Int expected frame size-} -> MSF m Bool (Bool, [Bool], Int, Int)
framer start len = mealy recognize restart
  where recognize markerbit ([h], go, [], n, frnum) | markerbit == h = ((False, [], n, frnum + 1), ([], go - 1, [], n, frnum + 1))  {- A bit belonging to the frame marker is found: strip it and continue; Do not accumulate it in the output frame-}
        recognize markerbit (h:t, go, [], n, frnum) | markerbit == h = ((not (null t), [], n, frnum), (t, go, [], n, frnum)) {- A bit belonging to the frame marker is found: strip it and continue; Do not accumulate it in the output frame-}
        recognize framebit ([], 0, fr, n, frnum) = let fr' = framebit:fr in ((False, fr', n + 1, frnum), ([], len, [], 0, frnum)) {-Current accumulated frame reached its given length; "Reset" the output and start again accumulating a new frame-}
        recognize framebit ([], go, fr, n, frnum) = let fr' = framebit:fr in ((False, fr', n + 1, frnum), ([], go - 1, fr', n + 1, frnum)) {-Accumulated frame, reduce its remaining expected length and increase the accumulated frame´s size-}
        recognize _ _ = ((True, [], 0, 0), restart) {-Until the frame marker is found declare LOF and do not accumulate bits-}
        restart :: ([Bool], Int, [Bool], Int, Int) {-Output´s start state: frame marker, expected frame length, accumulated frame, accumulated frame Length -}
        restart = (start, len, [], 0, 0)

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
