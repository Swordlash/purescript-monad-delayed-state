module Control.Monad.State.Delayed.Class
  ( class DelayedState
  , state
  , get
  , gets
  , put
  , modify
  , modify_
  ) where

import Prelude

import Data.Tuple (Tuple(..))

-- | Typeclass for a delayed state monad
class Monad m <= DelayedState s m | m -> s where
   state :: forall a. (s -> Tuple a s) -> m a

-- | Get the current state.
get :: forall m s. DelayedState s m => m s
get = state \s -> Tuple s s

-- | Get a value which depends on the current state.
gets :: forall s m a. DelayedState s m => (s -> a) -> m a
gets f = state \s -> Tuple (f s) s

-- | Set the state.
put :: forall m s. DelayedState s m => s -> m Unit
put s = state \_ -> Tuple unit s

-- | Modify the state by applying a function to the current state. The returned
-- | value is the new state value.
modify :: forall s m. DelayedState s m => (s -> s) -> m s
modify f = state \s -> let s' = f s in Tuple s' s'

modify_ :: forall s m. DelayedState s m => (s -> s) -> m Unit
modify_ f = state \s -> Tuple unit (f s)