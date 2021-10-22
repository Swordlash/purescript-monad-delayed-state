module Control.Monad.State.Delayed.Trans 
  ( DelayedStateT (..)
  , runDelayedStateT
  ) where

import Prelude
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Unlift (class MonadUnliftEffect)
import Control.Monad.State.Delayed.Delayer (Delayer(..), mkEmptyDelayerState, mkTimedOutDelayerState)

import Control.Monad.Base (class MonadBase, liftBase)
import Control.Monad.Reader (class MonadTrans, ReaderT(..), runReaderT)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.State.Delayed.Class (class DelayedState)
import Control.Monad.Trans.Control (class MonadBaseControl, class MonadTransControl, defaultLiftBaseWith, defaultRestoreM, liftBaseWith, restoreT)
import Data.Functor.Compose (Compose)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds, error, killFiber)
import Effect.Aff.AVar as AAVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Unsafe.Reference (unsafeRefEq)

newtype DelayedStateT :: Type -> (Type -> Type) -> Type -> Type
newtype DelayedStateT s m a = DelayedStateT (ReaderT (Delayer s) m a)

runDelayedStateT :: forall s a m. MonadAff m => Milliseconds -> DelayedStateT s m a -> m a
runDelayedStateT timeout (DelayedStateT reader) = do
  initialState <- liftAff $ mkEmptyDelayerState timeout
  runReaderT reader initialState

derive instance Newtype (DelayedStateT s m a) _
derive newtype instance MonadTrans (DelayedStateT s)
derive newtype instance Functor m => Functor (DelayedStateT s m)
derive newtype instance Applicative m => Applicative (DelayedStateT s m)
derive newtype instance Apply m => Apply (DelayedStateT s m)
derive newtype instance Bind m => Bind (DelayedStateT s m)
derive newtype instance Monad m => Monad (DelayedStateT s m)
derive newtype instance MonadEffect m => MonadEffect (DelayedStateT s m)
derive newtype instance MonadAff m => MonadAff (DelayedStateT s m)
derive newtype instance MonadBase b m => MonadBase b (DelayedStateT s m)
derive newtype instance MonadUnliftAff m => MonadUnliftAff (DelayedStateT s m)
derive newtype instance MonadUnliftEffect m => MonadUnliftEffect (DelayedStateT s m)

instance Monad m => MonadTransControl m (DelayedStateT s) Identity where
  liftWith f = DelayedStateT $ ReaderT $ \r -> f \(DelayedStateT (ReaderT g)) -> Identity <$> g r
  restoreT x = DelayedStateT $ restoreT x

instance (MonadBaseControl base m stM, Monad m, Monad base) => MonadBaseControl base (DelayedStateT s m) (Compose stM Identity) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance (MonadBaseControl Aff m stM, MonadState s m) => DelayedState s (DelayedStateT s m) where
  state f = DelayedStateT $ ReaderT $ \(Delayer timeout delayerStateVar) -> go timeout delayerStateVar
    where
      go timeout delayerStateVar = do
        delayerState <- liftBase $ AAVar.take delayerStateVar
        case delayerState of
          Nothing -> do
            baseState <- get
            let Tuple val newState = f baseState
            unless (unsafeRefEq baseState newState) $ handleStateChange newState -- it was `get`
            pure val
        
          Just { delayedState, commitFiber } -> do
            let Tuple val newState = f delayedState
            unless (unsafeRefEq delayedState newState) do
              liftBase $ killFiber (error "killed") commitFiber
              handleStateChange newState
            pure val
      
        where
          replaceAVar v var = AAVar.tryTake var *> AAVar.tryPut v var $> unit

          handleStateChange newState = do
            newDelayerState <- liftBaseWith 
                \runInBase -> mkTimedOutDelayerState timeout newState 
                  \s -> runInBase (put s) *> replaceAVar Nothing delayerStateVar
            liftBase $ AAVar.put (Just newDelayerState) delayerStateVar
