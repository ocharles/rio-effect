{-# language ConstraintKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module RIO.Effect.EFF
  ( EFF(..)
  , handleEffect
  ) where

-- base
import GHC.Exts ( Constraint )

-- rio
import RIO

-- rio-effects
import RIO.Effect.Handles ( Handles, Config, Requires, findHandler )
import RIO.Effect.Handler ( (:~>)(HandleWith), ($$) )

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


instance {-# OVERLAPPABLE #-} ( MonadTrans t, Monad ( t m ), EFF eff cfg m ) => EFF eff cfg ( t m ) where
  send =
    lift . send


instance ( Monad m, Handles eff r, Config eff r ~ cfg, Requires eff r ( ReaderT r m ) ) => EFF eff cfg ( ReaderT r m ) where
  send eff =
    asks ( findHandler @eff ) >>= \f -> f $$ send eff
  {-# INLINE send #-}


instance ( Handles eff r, Config eff r ~ cfg, Requires eff r ( RIO r ) ) => EFF eff cfg ( RIO r ) where
  send eff =
    asks ( findHandler @eff ) >>= \f -> f $$ send eff
  {-# INLINE send #-}


handleEffect
  :: forall c eff cfg.
     ( forall a. eff cfg a -> ( forall n. c n => n a ) )
  -> EFF eff cfg :~> c
handleEffect f =
  HandleWith
    ( \m ->
        runReaderT
          ( unHandle m )
          ( Interpreter f :: Interpreter eff cfg c )
    )


newtype Interpreter ( eff :: k -> * -> * ) ( cfg :: k ) ( c :: ( * -> * ) -> Constraint ) =
  Interpreter { interpret :: forall m a. c m => eff cfg a -> m a }


newtype HandlerT ( eff :: k -> * -> * ) ( cfg :: k ) ( c :: ( * -> * ) -> Constraint ) ( m :: * -> * ) ( a :: * ) =
  HandlerT { unHandle :: ReaderT ( Interpreter eff cfg c ) m a }
  deriving ( Functor, Applicative, Monad )


instance MonadTrans ( HandlerT eff cfg c ) where
  lift =
    HandlerT . lift


instance ( Monad m, c m, eff ~ eff', cfg ~ cfg' ) => EFF ( eff :: k -> * -> * ) ( cfg :: k ) ( HandlerT ( eff' :: k -> * -> * ) ( cfg' :: k ) c m ) where
  send e =
    HandlerT ( ReaderT ( \i -> interpret i e ) )
