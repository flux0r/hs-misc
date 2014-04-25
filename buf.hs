{-# LANGUAGE GADTs, Rank2Types, TypeOperators #-}

module Sf where

import Prelude hiding (init)

main :: IO ()
main = putStrLn "Hey!"

----------------------------------------------------------------------------

-- | Empty signal vector.
data Empty


-- | Singleton signal.
data Signal a


-- | Singleton event.
data Evt a


-- | Combine two signal vectors.
data Append l r



----------------------------------------------------------------------------
-- | Event.

data Occ v where
    Occ   :: forall a. a -> Occ (Evt a)
    OccL  :: Occ l -> Occ (Append l r)
    OccR  :: Occ r -> Occ (Append l r)



----------------------------------------------------------------------------
-- | Signal sample.

data Sample v where
    Sample      :: forall a. a -> Sample (Signal a)
    SampleEvt   :: Sample (Evt a)
    SampleEmpty :: Sample Empty
    SampleBoth  :: Sample vl -> Sample vr -> Sample (Append vl vr)



----------------------------------------------------------------------------
-- | Signal delta.

data Delta v where
    DeltaSignal     :: forall a. a -> Delta (Signal a)
    DeltaNothing    :: Delta v
    DeltaBoth       :: Delta vl -> Delta vr -> Delta (Append vl vr)



----------------------------------------------------------------------------
-- | Handlers.

data Handler out v where
    HandlerEmpty        :: Handler out Empty
    HandlerSignal       :: (a -> out) -> Handler out (Signal a)
    HandlerEvent        :: (a -> out) -> Handler out (Evt a)
    HandlerBoth         :: Handler out vl
                             -> Handler out vr
                             -> Handler out (Append vl vr)



----------------------------------------------------------------------------
-- | Event input.

data EventInput v where
    EvtIn       :: forall a. a -> EventInput (Evt a)
    EvtInL      :: EventInput vl -> EventInput (Append vl vr)
    EvtInR      :: EventInput vr -> EventInput (Append vl vr)



----------------------------------------------------------------------------
-- | Signal update.

data SignalUpdate v where
    SigUpdate       :: forall a. a -> SignalUpdate (Signal a)
    SigUpdateL      :: SignalUpdate vl -> SignalUpdate (Append vl vr)
    SigUpdateR      :: SignalUpdate vr -> SignalUpdate (Append vl vr)



----------------------------------------------------------------------------
-- | Initialization indicators.

-- | The signal function is either running or suspended.
data Initialized


-- The signal function isn't running yet.
data NonInitialized



----------------------------------------------------------------------------
-- | Signal function.
--
-- init     Distinguish between uninitialized and initialized signal 
--          functions. An unitialized function is just specified, while an 
--          initialized one is ready to respond to input and time.
-- vIn      Input signal vector.
-- vOut     Output signal vector.

data Sf init vIn vOut where
    Sf      :: (Sample vIn -> (Sample vOut, Sf Initialized vIn vOut))
            -> Sf NonInitialized vIn vOut     
    Init    :: (Double -> Delta vIn
                 -> (Delta vOut, [Occ vOut], Sf Initialized vIn vOut))
            -> (Occ vIn -> ([Occ vOut], Sf Initialized vIn vOut))
            -> Sf Initialized vIn vOut



type vIn :~> vOut = Sf NonInitialized vIn vOut
type vl :^: vr = Append vl vr

identity :: v :~> v
identity = Sf (\init -> (init, identityInit))

identityInit :: Sf Initialized v v
identityInit = Init (\_ delta -> (delta, [], identityInit)) (\evt -> ([evt], identityInit))
