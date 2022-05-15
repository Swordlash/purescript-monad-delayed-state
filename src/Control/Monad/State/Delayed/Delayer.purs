module Control.Monad.State.Delayed.Delayer where

import Effect.AVar (AVar)
import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Fiber, Milliseconds, delay, forkAff)
import Effect.Aff.AVar as AAVar

data Delayer s = Delayer Milliseconds (AVar (Maybe (DelayerState s)))

type DelayerState s =
  { delayedState :: s
  , commitFiber  :: Fiber Unit
  }

mkEmptyDelayerState :: forall s. Milliseconds -> Aff (Delayer s)
mkEmptyDelayerState millis = Delayer millis <$> AAVar.new Nothing

mkTimedOutDelayerState :: forall s. Milliseconds -> s -> (s -> Aff Unit) -> Aff (DelayerState s)
mkTimedOutDelayerState millis delayedState commitState = do
  commitFiber <- forkAff do
    delay millis
    commitState delayedState
  pure { delayedState, commitFiber }
