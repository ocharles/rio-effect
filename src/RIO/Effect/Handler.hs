{-# language ConstraintKinds #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}

module RIO.Effect.Handler ( (:~>)(..), handleEffect ) where


newtype (:~>) eff constraints =
  HandleWith { ($$) :: forall m x. constraints m => eff x -> m x }


handleEffect
  :: forall c eff.
     ( forall m x. c m => eff x -> m x )
  -> eff :~> c
handleEffect =
  HandleWith
