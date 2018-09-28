{-# language ConstraintKinds #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}

module RIO.Effect.Handler ( (:~>)(..) ) where

import Control.Category
import Prelude hiding ( (.), id )


newtype c :~> c' =
  HandleWith { ($$) :: forall x. ( forall m. ( Monad m, c m ) => m x ) -> ( forall n. ( Monad n, c' n ) => n x ) }


instance Category (:~>) where
  id =
    HandleWith ( \m -> m )

  HandleWith g . HandleWith f =
    HandleWith ( \m -> g ( f m ) )
