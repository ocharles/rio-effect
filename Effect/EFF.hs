{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Effect.EFF ( EFF(..) ) where

-- reader-effects
import Effect.Handles ( Handles, Config, Requires, findHandler )
import Effect.Handler

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader


class Monad m => EFF eff cfg m | eff m -> cfg where
  send :: eff cfg a -> m a

  default send :: ( MonadTrans t, m ~ t n, EFF eff cfg n ) => eff cfg a -> m a
  send =
    lift . send


instance ( Monad m, Handles eff r, Config eff r ~ cfg, Requires eff r ( ReaderT r m ) ) => EFF eff cfg ( ReaderT r m ) where
  send eff =
    asks findHandler >>= \f -> f $$ eff
  {-# INLINE send #-}
