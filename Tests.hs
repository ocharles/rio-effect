{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Effect where

import Data.Type.Bool
import Data.Type.Equality
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import GHC.Exts
import GHC.Generics


newtype (:~>) ( eff :: * -> * ) constraints =
  HandleWith { ($$) :: forall m x. constraints m => eff x -> m x }


class Monad m => EFF eff cfg m | eff m -> cfg where
  send :: eff cfg a -> m a

  default send :: ( MonadTrans t, m ~ t n, EFF eff cfg n ) => eff cfg a -> m a
  send =
    lift . send


class Handles eff a where
  type Config eff a :: *

  type Requires eff a :: ( * -> * ) -> Constraint

  findHandler :: a -> eff ( Config eff a ) :~> Requires eff a

  type Config eff a = GConfig eff ( Rep a )

  type Requires eff a = GRequires eff ( Rep a )

  default findHandler :: ( Generic a, GFindHandler ( Rep a ) eff cfg c, ( Config eff a ) ~ cfg, Requires eff a ~ c ) => a -> eff ( Config eff a ) :~> Requires eff a
  findHandler =
    gfindHandler . from


class GFindHandler f eff cfg c where
  type GRequires eff f :: ( * -> * ) -> Constraint

  type GConfig eff f :: *

  gfindHandler :: f a -> eff cfg :~> c


instance GFindHandler f eff cfg c => GFindHandler ( M1 i z f ) eff cfg c where
  type GRequires eff ( M1 i z f ) = GRequires eff f

  type GConfig eff ( M1 i z f ) = GConfig eff f

  gfindHandler ( M1 a ) = gfindHandler a


type family HasEff ( eff :: * -> * -> * ) ( f :: * -> * ) :: Bool where
  HasEff eff ( K1 i ( f cfg :~> c ) ) = eff == f
  HasEff eff ( K1 i a ) = 'False
  HasEff eff ( M1 i z f ) = HasEff eff f
  HasEff eff ( f :*: g ) = HasEff eff f || HasEff eff g


instance ( If ( HasEff eff f ) ( GFindHandler f eff cfg c ) ( GFindHandler g eff cfg c ), SBool ( HasEff eff f ) ) => GFindHandler ( f :*: g ) eff cfg c where
  type GRequires eff ( f :*: g ) =
    If ( HasEff eff f ) ( GRequires eff f ) ( GRequires eff g )

  type GConfig eff ( f :*: g ) =
    If ( HasEff eff f ) ( GConfig eff f ) ( GConfig eff g )

  gfindHandler ( f :*: g ) =
    case sbool @(HasEff eff f) of
      STrue -> gfindHandler f
      SFalse -> gfindHandler g


data SomeBool :: Bool -> * where
  STrue :: SomeBool 'True
  SFalse :: SomeBool 'False


class SBool ( b :: Bool ) where
  sbool :: SomeBool b

instance SBool 'True where sbool = STrue
instance SBool 'False where sbool = SFalse


instance ( eff' ~ eff, cfg' ~ cfg, c' ~ c ) => GFindHandler ( K1 i ( eff' ( cfg' :: * ) :~> c' ) ) eff cfg c where
  type GRequires eff ( K1 i ( eff' cfg' :~> c' ) ) = c'

  type GConfig eff ( K1 i ( eff' cfg' :~> c' ) ) = cfg'

  gfindHandler ( K1 a ) = a


instance ( f ~ g ) => Handles f ( g cfg :~> c ) where
  type Config f ( g cfg :~> c ) =
    cfg

  type Requires f ( g cfg :~> c ) =
    c

  findHandler =
    id


instance ( Monad m, Handles eff r, Config eff r ~ cfg, Requires eff r ( ReaderT r m ) ) => EFF eff cfg ( ReaderT r m ) where
  send eff = asks findHandler >>= \f -> f $$ eff
