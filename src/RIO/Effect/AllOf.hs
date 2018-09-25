{-# language UndecidableSuperClasses #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module RIO.Effect.AllOf ( AllOf ) where

-- base
import GHC.Exts ( Constraint )


type family Apply ( cs :: [ ( * -> * ) -> Constraint ] ) ( m :: * -> * ) :: Constraint where
  Apply '[] m = ()
  Apply ( c ': cs ) m = ( c m, Apply cs m )


class Apply cs m => AllOf cs m


instance Apply cs m => AllOf cs m
