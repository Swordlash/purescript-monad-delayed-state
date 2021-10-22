module Test.Main where

import Prelude

import Control.Monad.Base (class MonadBase)
import Control.Monad.Reader (class MonadTrans, ReaderT(..), runReaderT)
import Control.Monad.State (class MonadState, get)
import Control.Monad.State.Delayed.Class (modify_)
import Control.Monad.State.Delayed.Trans (runDelayedStateT)
import Control.Monad.Trans.Control (class MonadBaseControl, class MonadTransControl, defaultLiftBaseWith, defaultRestoreM, liftBaseWith, restoreT)
import Data.Functor.Compose (Compose)
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Assert (assert')

---------------------------------------------------------------

newtype DummyState :: Type -> (Type -> Type) -> Type -> Type
newtype DummyState s m a = DummyState (ReaderT (AVar s) m a)

derive instance Newtype (DummyState s m a) _
derive newtype instance Functor m => Functor (DummyState s m)
derive newtype instance Applicative m => Applicative (DummyState s m)
derive newtype instance Apply m => Apply (DummyState s m)
derive newtype instance Bind m => Bind (DummyState s m)
derive newtype instance Monad m => Monad (DummyState s m)
derive newtype instance MonadEffect m => MonadEffect (DummyState s m)
derive newtype instance MonadAff m => MonadAff (DummyState s m)

derive newtype instance MonadBase b m => MonadBase b (DummyState s m)
derive newtype instance MonadTrans (DummyState s)

instance Monad m => MonadTransControl m (DummyState s) Identity where
  liftWith f = DummyState $ ReaderT $ \r -> f \(DummyState (ReaderT g)) -> Identity <$> g r
  restoreT x = DummyState $ restoreT x

instance (MonadBaseControl base m stM, Monad m, Monad base) => MonadBaseControl base (DummyState s m) (Compose stM Identity) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadAff m => MonadState s (DummyState s m) where
  state f = DummyState $ ReaderT $ \avar -> do
    var <- liftAff $ AVar.take avar
    let Tuple v nvar = f var
    liftAff $ AVar.put nvar avar
    pure v

forkDummyAff :: forall s a m stM. MonadBaseControl Aff m stM => DummyState s m a -> DummyState s m (Fiber Unit)
forkDummyAff action = liftBaseWith \runInBase -> forkAff (void $ runInBase action)

runDummy :: forall s a. s -> DummyState s Aff a -> Aff a
runDummy initial (DummyState reader) = do
  initialVar <- AVar.new initial
  runReaderT reader initialVar

main :: Effect Unit
main = launchAff_ $ runDummy 0 $ do
  void $ forkDummyAff $ runDelayedStateT (Milliseconds 100.0) $ do
    modify_ (_ + 1)
    liftAff $ delay (Milliseconds 40.0)
    modify_ (_ + 1)
    liftAff $ delay (Milliseconds 120.0)
    modify_ (_ + 1)
    liftAff $ delay (Milliseconds 120.0)
    modify_ (_ + 1)
  assertState 0
  liftAff $ delay (Milliseconds 50.0)
  assertState 0
  liftAff $ delay (Milliseconds 100.0)
  assertState 2
  liftAff $ delay (Milliseconds 100.0)
  assertState 2
  liftAff $ delay (Milliseconds 100.0)
  assertState 3
  liftAff $ delay (Milliseconds 100.0)
  assertState 4
  
  where
    assertState expected = do
      state <- get
      liftEffect $ assert' ("Error: expected " <> show expected <> ", but got " <> show state) (expected == state)