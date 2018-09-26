{-# language PolyKinds #-}
{-# language TypeOperators #-}

module RIO.Effect
  ( -- $intro

    -- * Getting Started
    -- $gettingStarted

    -- * API
    -- ** Using effects
    EFF
  , send

    -- ** Effect handlers
  , (:~>)
  , ($$)
  , handleEffect
  , AllOf

    -- ** Effectful environments
  , Handles
  , runEff
  ) where

-- rio-effect
import RIO.Effect.AllOf
import RIO.Effect.EFF
import RIO.Effect.Handler ( (:~>), ($$), handleEffect )
import RIO.Effect.Handles

-- transformers
import Control.Monad.Trans.Reader ( ReaderT, runReaderT )


{- $intro

Welcome to @reader-effects@. @reader-effects@ is a small effect handling library
that is similar in essence to the monad transformer library - @mtl@. In
@reader-effects@, you work with a single @EFF@ type class to specify that a
monadic computation needs access to particular effects, and later you will
run that monadic computation in a particular environment that contains a
collection of appropriate effect handlers.

The name @reader-effects@ is meant to emphasize the connection between this and
the reader monad transformer - @r -> m a@. Here, our environment @r@ is a
collection of effect handlers, and the @send@ method from @EFF@ simply finds
the appropriate handler for an effect.

-}


{- $gettingStarted

There's no better substitute for learning something than to get your hands dirty
and try it yourself, so feel free to code along with this little getting started
guide. We'll start by building a small "teletype", which provides access to
@STDIN@ and @STDOUT@.

== Describing the effect

To start, we'll define the operations that our teletype effect provides. This is
done by defining a generalized algebraic data type - GADT - that describes the
type signatures of effect operations.

@
\{\-\# LANGUAGE GADTs \#\-\}

data Teletype config a where
  GetLine :: Teletype config String
  PutLine :: String -> Teletype config ()
@

This data type shows that the @Teletype@ effect gives us access to @GetLine@ and
@PutLine@ operations. @GetLine@ doesn't take any arguments, but will produce a
@String@, and @PutLine@ takes a @String@ but return @()@. Don't worry about
@Config@ for now (that is used in parameterized effects).

To use this effect in a computation, we need to @send@ the operations into the
monad - a little like how we use @liftIO@ to lift IO actions into general
monads. Rather than making our uses depend on @reader-effects@ and write @send@
everywhere, we'll provide a more convenient interface:

> import Effect (EFF, send)
>
> getLine :: EFF Teletype config m => m String
> getLine = send GetLine
>
> putLine :: EFF Teletype config m => String -> m ()
> putLine l = send (PutLine l)

To demonstrate the use of this effect, let's write a small program that uses
it.

> authenticate :: EFF Teletype config m => m Bool
> authenticate = do
>   putLine "Please enter your username:"
>   user <- getLine
>   putLine "Please enter your password:"
>   password <- getLine
>
>   return (user == "root" && password == "Z1ON0101")

== Effect Handlers

Now that we have a description of our effect, we need to provide users with a
way to run the effect. This is done by providing an /effect handler/. Effect
handlers are built by essentially pattern matching on @Teletype@, and writing
a corresponding monadic action that may use other effects. To begin, let's
write a handler that uses the standard @getLine@ and @putStrLn@ functions
from the @Prelude@:

@
\{\-\# LANGUAGE TypeOperators \#\-\}

import Control.Monad.IO.Class (liftIO)
import Effect ( (:~>) )

teletypeIO :: Teletype config :~> MonadIO
teletypeIO = handleEffect $ \eff ->
  case eff of
    GetLine -> liftIO Prelude.getLine
    PutLine l -> liftIO (Prelude.putStrLn l)
@

The type of this interpret shows that we'll reinterpret the @Teletype@ effect
into anything that supports @MonadIO@. The implementation itself uses
@handleEffect@ to build the handler, which needs a function of the type
@forall m a. MonadIO m => Teletype config a -> m a@, which we provide by
pattern matching on the effect we need to handle.

== Running effectful computations

Finally, we'll put everything together and run our @authenticate@ program. To
do so, we'll use a reader monad over IO with @runReaderT@, and our environment
will be the @teletypeIO@ handler:

> main :: IO ()
> main = runReaderT authenticate teletypeIO

-}


runEff :: eff cfg :~> c -> ReaderT ( eff cfg :~> c ) m a -> m a
runEff =
  flip runReaderT
