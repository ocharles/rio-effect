{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module RIO.Effect.EFF
  ( EFF(..)
  ) where

-- rio
import RIO

-- rio-effects
import RIO.Effect.Handles ( Handles, Config, Requires, findHandler )
import RIO.Effect.Handler ( ($$) )

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader ( ReaderT )
import qualified Control.Monad.Trans.State.Strict as StrictState
import qualified Control.Monad.Trans.State.Lazy as LazyState


class Monad m => EFF eff cfg m | eff m -> cfg where
  send :: eff cfg a -> m a

  default send :: ( MonadTrans t, m ~ t n, EFF eff cfg n ) => eff cfg a -> m a
  send =
    lift . send


instance ( Monad m, Handles eff r, Config eff r ~ cfg, Requires eff r ( ReaderT r m ) ) => EFF eff cfg ( ReaderT r m ) where
  send eff =
    asks findHandler >>= \f -> f $$ eff
  {-# INLINE send #-}


instance ( EFF eff cfg m, Monad m ) => EFF eff cfg ( ExceptT e m )


instance ( EFF eff cfg m, Monad m ) => EFF eff cfg ( MaybeT m )


instance ( EFF eff cfg m, Monad m ) => EFF eff cfg ( StrictState.StateT e m )


instance ( EFF eff cfg m, Monad m ) => EFF eff cfg ( LazyState.StateT e m )


instance ( Handles eff r, Config eff r ~ cfg, Requires eff r ( RIO r ) ) => EFF eff cfg ( RIO r ) where
  send eff =
    asks findHandler >>= \f -> f $$ eff
  {-# INLINE send #-}
