{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module RIO.Effect
  ( -- * Getting Started
    -- ** Defining Effects
    -- $defining

    -- * Introducing Effects
    EFF
  , send

    -- * Handling Effects
  , (:~>)( ($$) )
  , handleEffect

   -- * Eliminating single effects
  , Eff
  , runEff
  ) where

-- base
import Data.Proxy ( Proxy(..) )
import Data.Type.Equality
import GHC.Exts ( Constraint )

-- rio
import RIO hiding ( handle )

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader ( ReaderT(..) )
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict

{- $defining

To define effects, we begin by defining a GADT (generalized algebraic data type)
that describes the operations provided by our effect. For this getting started
guide, we'll build an effect for interacting with Twitter. Our effect will allow
us to post a tweet and list recent tweets.

@
data Twitter cfg a where
  PostTweet :: String -> Twitter () ()
  ListTweets :: Twitter () [Tweet]
@

Here we see that @PostTweet@ takes a @String@ and returns @()@, whereas
@ListTweets@ takes no arguments and returns a list of @Tweet@s. We also see
a @cfg@ type parameter. This is to allow parameterized effects (discussed
later), but is not important for the Twitter effect, so we fix @cfg = ()@
in all effect operations.

With these operations defined, we can also provide a more convenient interface
to our effects:

@
postTweet :: EFF Twitter () m => String -> m ()
postTweet message = send ( PostTweet message )

listTweets :: EFF Twitter () m => m [Tweet]
listTweets = send ListTweets
@

'send' takes an effect operation and sends it out for implementation by a
handler. Essentially, what we are doing is packaging up all the interesting
information an implementation would use, without actually committing to any
particular implementation.

$handling

Now that we have an effect, we need to provide an implementation or /handler/
for the effect.

Handlers are built using 'handleEffect', and pattern match on effect
operations. A handler also has access to other type-class based effects,
including @EFF@ itself. Here is an implementation that uses a state monad.

-}

{-| The class of monads that support effects of type @eff@, configured with
    parameters @params@. Note that due to the functional dependency, an effect
    can only occur once per monad, though this leads to significantly better
    type inference.
-}
class Monad m => EFF eff params m | m eff -> params where
  send :: eff params a -> m a


{-|

A handler for an effect with signature @f@. @constraints@ is a list of
monadic constraints that the handler requires.

-}
newtype (:~>) ( eff :: * -> * ) constraints =
  HandleWith { ($$) :: forall m x. ( Monad m, MonadConstraints constraints m ) => eff x -> m x }


{-|

This type family takes a list of @MonadFoo@ constraints and constraints a
monad @m@ against them.

> MonadConstraints '[ MonadIO, MonadState s ] m = ( MonadIO m, MonadState s m )

-}
type family MonadConstraints ( cs :: [ ( * -> * ) -> Constraint ] ) ( m :: * -> * ) :: Constraint where
  MonadConstraints '[] m =
    ()

  MonadConstraints ( c ': cs ) m =
    ( c m, MonadConstraints cs m )


{-|

Handle effect builds an effect handler for effects of signature @f@, by
re-interpreting it into a set of monadic constraints given by @constraints@.

As an example, here is the definition of an effect that greets people,
along with a handler into an 'MonadIO':

> data Greet config a where
>   Greet :: { name :: String } -> Greet config ()
>
> greeter :: Greet () :~> '[ MonadIO ]
> greeter =
>   handleEffect ( \( Greet { name } ) -> putStrLn ( "Hello, " ++ name ++ "!" ) )

-}
handleEffect
  :: forall constraints f.
     ( forall m x. ( Monad m, MonadConstraints constraints m ) => f x -> m x )
  -> f :~> constraints
handleEffect =
  HandleWith


{-|

Find an effect handler in a given environment.

-}
class Handles ( eff :: k -> * -> * ) ( params :: k ) env | eff env -> params where
  type Constraints eff params env :: [ ( * -> * ) -> Constraint ]

  findHandler :: env -> eff params :~> Constraints eff params env


instance ( MonadConstraints ( Constraints eff params env ) ( RIO env ), Handles eff params env ) => EFF eff params ( RIO env ) where
  send eff = do
    env <-
      ask

    findHandler env $$ eff


instance EFF eff x m => EFF eff x ( ExceptT e m ) where
  send =
    lift . send


instance EFF eff x m => EFF eff x ( Lazy.StateT s m ) where
  send =
    lift . send


instance EFF eff x m => EFF eff x ( Strict.StateT s m ) where
  send =
    lift . send


instance Handles eff x ( eff x :~> cs ) where
  type Constraints eff x ( eff x :~> cs ) = cs

  findHandler =
    id


data MISSING


type family LeftEffect ( a :: * ) :: * where
  LeftEffect ( f x :~> cs ) = f x ()
  LeftEffect _ = MISSING


instance ( HandleChoice ( eff x == eff' x ) ( eff' x :~> cs ) r eff x, l ~ ( eff' x :~> cs ) ) => Handles eff x ( l, r ) where
  type Constraints eff x ( l, r ) =
    ConstraintLR ( eff x () == LeftEffect l ) l r eff x

  findHandler =
    findHandlerChoice ( Proxy @( eff x () == LeftEffect l ) )


class HandleChoice ( isLeft :: Bool ) l r ( eff :: * -> * -> * ) x where
  type ConstraintLR isLeft l r eff x :: [ ( * -> * ) -> Constraint ]

  findHandlerChoice :: proxy isLeft -> ( l, r ) -> eff x :~> ConstraintLR isLeft l r eff x


type family ConstraintsOf ( a :: * ) :: [ ( * -> * ) -> Constraint ] where
  ConstraintsOf ( f x :~> cs ) = cs


instance ( l ~ ( eff x :~> ConstraintsOf l ) ) => HandleChoice 'True l r eff x where
  type ConstraintLR 'True l r eff x = ConstraintsOf l

  findHandlerChoice _ ( handler, _ ) =
    handler


instance Handles eff x r => HandleChoice 'False l r eff x where
  type ConstraintLR 'False l r eff x = Constraints eff x r

  findHandlerChoice _ ( _, r ) =
    findHandler r


newtype Eff eff cs m a = Eff ( ReaderT ( eff :~> cs ) m a )
  deriving ( Functor, Applicative, Monad )


runEff :: MonadConstraints cs m => eff :~> cs -> Eff eff cs m a -> m a
runEff handler ( Eff m ) =
  runReaderT m handler


instance ( f ~ g, Monad m, MonadConstraints cs m ) => EFF f x ( Eff ( g x ) cs m ) where
  send eff =
    Eff ( ReaderT ( $$ eff ) )
