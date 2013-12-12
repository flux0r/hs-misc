{-# LANGUAGE NoImplicitPrelude #-}

import Control.Monad.State (MonadState)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word)

-- A circuit is a directed graph of components with wires as edges. The
-- components have input and output pins and each wire takes an output of one
-- component and connects it to an input of another component.
--
-- We need to be able to do the following.
--       *  Identify intended input and output pins
--       *  Make sure the graph is fully connected (except for the top-level
--          inputs and outputs)
--       *  Make sure only one particular output pin drives a particular input
--          pin, while allowing a particular output pin to drive an arbitrary
--          number of unique input pins
--       *  Compose the graphs, matching up free outputs and inputs
--
-- To meet the composition goal, we can note that programming lenguages deal
-- with this by representing inputs as function parameters and outputs as
-- function bodies. Instead of writing graph nodes and edges directly,
-- we'll represent circuits as functions that consume output pins, generate
-- a circuit fragment, and indicate the output pins for that fragment.
--
-- newtype CircuitG a = CircuitG (OutputPins -> (OutputPins, [Component], a))
--
-- CircuitG is an example of a pattern using a writer monad and a state monad.

--newtype Circuit 

-- A primitive is identified by a name.
newtype Primitive a b = Primitive Text

-- We need to be able to indentify pins and construct new ones, so we'll just
-- use Word wrapped in a newtype.
newtype Pin = Pin Word
newtype Pins = Pins [Pin]

newtype PinsPair a b = PinsPair (HasPins a, HasPins b)

-- We need to be able flatten pins into a sequence, generate new pins for the
-- outputs of a new instance, and get the number of pins.
class HasPins a where
    to_pins     :: a -> Seq Pin
    mk_pins     :: MonadPins f => f a
    num_pins    :: a -> Word

newtype MonadPins f = MonadPins (MonadState Pins f)
