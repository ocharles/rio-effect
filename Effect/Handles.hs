{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Effect.Handles ( Handles(..) ) where

-- base
import Data.Type.Bool
import GHC.Exts
import GHC.Generics

-- reader-effects
import Effect.Handler

-- singleton-bool
import Data.Singletons.Bool


class Handles eff a where
  type Config eff a :: *

  type Requires eff a :: ( * -> * ) -> Constraint

  findHandler :: a -> eff ( Config eff a ) :~> Requires eff a

  type Config eff a = GConfig eff ( Rep a )

  type Requires eff a = GRequires eff ( Rep a )

  default findHandler :: ( Generic a, GFindHandler ( Rep a ) eff cfg c, ( Config eff a ) ~ cfg, Requires eff a ~ c ) => a -> eff ( Config eff a ) :~> Requires eff a
  findHandler =
    gfindHandler . from


instance ( f ~ g ) => Handles f ( g cfg :~> c ) where
  type Config f ( g cfg :~> c ) =
    cfg

  type Requires f ( g cfg :~> c ) =
    c

  findHandler =
    id
  {-# INLINE findHandler #-}


type family ProductHasEff ( eff :: * -> * -> * ) ( a :: * ) :: Bool where
  ProductHasEff eff ( eff _ :~> _, _ ) = 'True
  ProductHasEff _ _ = 'False


instance ( If ( ProductHasEff eff ( l, r ) ) ( Handles eff l ) ( Handles eff r ), SBoolI ( ProductHasEff eff ( l, r ) ) ) => Handles eff ( l, r ) where
  type Requires eff ( l, r ) =
    If ( ProductHasEff eff ( l, r ) ) ( Requires eff l ) ( Requires eff r )

  type Config eff ( l, r ) =
    If ( ProductHasEff eff ( l, r ) ) ( Config eff l ) ( Config eff r )

  findHandler ( l, r ) =
    case sbool @( ProductHasEff eff ( l, r ) ) of
      STrue -> findHandler l
      SFalse -> findHandler r


instance ( If ( ProductHasEff eff ( a1, ( a2, a3 ) ) ) ( Handles eff a1 ) ( Handles eff ( a2, a3 ) ), SBoolI ( ProductHasEff eff ( a1, ( a2, a3 ) ) ) ) => Handles eff ( a1, a2, a3 ) where
  type Requires eff ( a1, a2, a3 ) =
    If ( ProductHasEff eff ( a1, ( a2, a3 ) ) ) ( Requires eff a1 ) ( Requires eff ( a2, a3 ) )

  type Config eff ( a1, a2, a3 ) =
    If ( ProductHasEff eff ( a1, ( a2, a3 ) ) ) ( Config eff a1 ) ( Config eff ( a2, a3 ) )

  findHandler ( a1, a2, a3 ) =
    findHandler ( a1, ( a2, a3 ) )


type family HasEff ( eff :: * -> * -> * ) ( f :: * -> * ) :: Bool where
  HasEff eff ( K1 _ ( eff _ :~> _ ) ) = 'True
  HasEff eff ( M1 _ _ f ) = HasEff eff f
  HasEff eff ( f :*: g ) = HasEff eff f || HasEff eff g
  HasEff _ _ = 'False


class GFindHandler f eff cfg c where
  type GRequires eff f :: ( * -> * ) -> Constraint

  type GConfig eff f :: *

  gfindHandler :: f a -> eff cfg :~> c


instance GFindHandler f eff cfg c => GFindHandler ( M1 i z f ) eff cfg c where
  type GRequires eff ( M1 i z f ) = GRequires eff f

  type GConfig eff ( M1 i z f ) = GConfig eff f

  gfindHandler ( M1 a ) = gfindHandler a
  {-# INLINE gfindHandler #-}


instance ( If ( HasEff eff f ) ( GFindHandler f eff cfg c ) ( GFindHandler g eff cfg c ), SBoolI ( HasEff eff f ) ) => GFindHandler ( f :*: g ) eff cfg c where
  type GRequires eff ( f :*: g ) =
    If ( HasEff eff f ) ( GRequires eff f ) ( GRequires eff g )

  type GConfig eff ( f :*: g ) =
    If ( HasEff eff f ) ( GConfig eff f ) ( GConfig eff g )

  gfindHandler ( f :*: g ) =
    case sbool @(HasEff eff f) of
      STrue -> gfindHandler f
      SFalse -> gfindHandler g
  {-# INLINE gfindHandler #-}


instance ( eff' ~ eff, cfg' ~ cfg, c' ~ c ) => GFindHandler ( K1 i ( eff' ( cfg' :: * ) :~> c' ) ) eff cfg c where
  type GRequires eff ( K1 i ( eff' cfg' :~> c' ) ) = c'

  type GConfig eff ( K1 i ( eff' cfg' :~> c' ) ) = cfg'

  gfindHandler ( K1 a ) = a
  {-# INLINE gfindHandler #-}
