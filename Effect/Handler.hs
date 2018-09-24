{-# language ConstraintKinds #-}
{-# language RankNTypes #-}
{-# language TypeOperators #-}

module Effect.Handler ( (:~>)(..) ) where


newtype (:~>) eff constraints =
  HandleWith { ($$) :: forall m x. constraints m => eff x -> m x }
