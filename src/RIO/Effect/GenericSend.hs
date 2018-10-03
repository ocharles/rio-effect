{-# language AllowAmbiguousTypes #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module RIO.Effect.GenericSend ( genericSend ) where

-- base
import GHC.Generics ( Generic, Rep, from, to, M1(..), (:*:)(..), K1(..) )
import Data.Proxy
import GHC.Exts ( Constraint )

-- rio-effect
import RIO.Effect.EFF ( EFF, send )


genericSend
  :: forall ( m :: * -> * ) constructors senders.
     ( Send m ( Rep constructors ) ~ Rep senders
     , Generic senders
     , Generic constructors
     , CS ( Rep constructors ) m
     , GenericSend ( Rep constructors )
     )
  => constructors -> senders
genericSend =
  to . genericSend_ ( Proxy @m ) . from


class GenericSend constructors where
  type Send ( m :: * -> * ) constructors :: * -> *

  type CS constructors ( m :: * -> * ) :: Constraint

  genericSend_ :: CS constructors m => proxy m -> constructors a -> Send m constructors x


instance GenericSend f => GenericSend ( M1 i c f ) where
  type Send m ( M1 i c f ) =
    M1 i c ( Send m f )

  type CS ( M1 i c f ) m =
    CS f m

  genericSend_ p ( M1 a ) =
    M1 ( genericSend_ p a )


instance ( GenericSend f, GenericSend g ) => GenericSend ( f :*: g ) where
  type Send m ( f :*: g ) =
    ( Send m f :*: Send m g )

  type CS ( f :*: g ) m =
    ( CS f m, CS g m )

  genericSend_ p ( a :*: b ) =
    genericSend_ p a :*: genericSend_ p b


instance GenericSend ( K1 i c ) where
  type Send m ( K1 i c ) =
    K1 i ( SendType m c )

  type CS ( K1 i c ) m =
    GenericSendF c m ( SendType m c )

  genericSend_ p ( K1 a ) =
    K1 ( toSend a )


type family SendType ( m :: * -> * ) ( c :: * ) :: * where
  SendType m ( x -> a ) = x -> SendType m a
  SendType m ( f a ) = m a
  SendType m a = m a


type family C ( m :: * -> * ) ( a :: * ) :: Constraint where
  C m ( x -> a ) = C m a
  C m ( eff cfg a ) = ( EFF eff cfg m )


class GenericSendF a ( m :: * -> * ) r | a m -> r, r -> m where
  toSend :: a -> r


instance EFF eff cfg m => GenericSendF ( eff cfg a ) m ( m a ) where
  toSend a =
    send a


instance GenericSendF b m c => GenericSendF ( a -> b ) m ( a -> c ) where
  toSend a x =
    toSend ( a x )
