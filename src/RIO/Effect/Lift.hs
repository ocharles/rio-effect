{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module RIO.Effect.Lift where

import Control.Monad.Trans.Class


newtype Free c a =
  Free { unFree :: forall m. ( Monad m, c m ) => m a }


class Monad m => Lift c m where
  liftFree :: Free c a -> m a


instance ( Monad m, MonadTrans t, c m, Monad ( t m ) ) => Lift c ( t m ) where
  liftFree ( Free m ) =
    lift m


relay :: forall c m a. Lift c m => ( forall n. ( Monad n, c n ) => n a ) -> m a
relay m =
  liftFree @c @m ( Free m )
