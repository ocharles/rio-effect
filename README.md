# `rio-effect`

A note on the name: originally this project was quite tied to RIO, but I've
since discovered it's more general than that. On release, this will probably be
reader-effects or something. I'm open to name suggestions!

## reader-effects demo

In this demo, we'll look at building the traditional "teletype" example using
`rio-effect`. `rio-effect` is a slightly different type of extensible effects
framework that is less powerful than others - it doesn't support special return
types for effects like `Either` or `Maybe`, and it doesn't give effect handlers
access to continuations to build effects like non-determination. That said,
whether this is a problem *in practice* is something to explore.

The idea of `rio-effect` is to work with a reader-like monad that has a record
of effect handlers. These handlers are algebras for an effect signature,
handling the effect by reinterpreting it into other monad classes. Handlers can
be combined, allowing GHC to essentially fuse effect handlers. These handles
will often compose down to `Lift MonadIO`, which means that the base monad must
support `MonadIO`. Likewise, you could imagine compiling to `Lift MonadFail` to
give access to early termination by stacking the effect handlers over MaybeT.

Enough waffling, on with the demo!

First, some necessary language extensions

``` haskell
{-# language FlexibleContexts #-} -- Needed for the type of program
{-# language GADTs #-}            -- Needed to define our effect signature
{-# language LambdaCase #-}       -- Needed by teletypeIO, but we could use a normal case statement
{-# language TemplateHaskell #-}  -- Needed to use genSend, not essential
{-# language TypeOperators #-}    -- Needed for :~>. A named version could always be added.
```

And some imports...

``` haskell
-- We'll need more general composition operators
import Prelude hiding ( (.), id )
import Control.Category ( (.), id )

-- We'll write our effect into MonadIO
import Control.Monad.IO.Class ( MonadIO, liftIO )

import RIO.Effect
import RIO.Effect.TH
```

First we define our effect operations. This is like extensible-effects, but we
require an extra "configuration" parameter for parameterized effects. `Teletype`
doesn't use this, so it leaves it polymorphic in effects. This is used to give
much better inference to things like `State s` effects.

``` haskell
data Teletype cfg a where
  GetL :: Teletype cfg String         -- No arguments, returns String
  PutL :: String -> Teletype cfg ()   -- Takes a String, returns ()
```

Next, use Template Haskell to derive a more convenient API:

``` haskell
genSend ''Teletype
```

This gives us:

``` haskell
getL :: EFF Teletype cfg m => m String
getL = send GetL
```

Etc.

With our effect defined, we can now write programs that needs access to `EFF
Teletype ()`. These are just a normal mtl-like programs, polymorphic over any
monad m (that implements `EFF Teleteype ()`).

``` haskell
program :: EFF Teletype () m => m String
program = do
  putL "What is your name?"
  getL
```

Next, define an interpreter this effect into any `MonadIO` monad. This is just
pattern matching on the above constructors, and providing a corresponding
`MonadIO` action to run.

``` haskell
teletypeIO :: EFF Teletype cfg :~> MonadIO
teletypeIO =
  handleEffect $ \case
    GetL -> liftIO getLine
    PutL l -> liftIO ( putStrLn l )
```

Run it! We give our program to handleEffects, and pass it an interpreter for
`EFF Teletype ()`. We have an interpreter `EFF Teletype () :~> MonadIO`, so
we'll relay the MonadIO constraint to our base monad - `IO`. This is done with
`liftHandler :: a :~> Lift a`.

(This isn't actually necessary as `handleEffects` uses a monad transformer that
already handles `MonadIO` anyway. However, if we use an effect that
`handleEffects` can't understand (e.g., `MonadLog` from some logging framework),
then liftHandler will get us out of that rut.)

``` haskell
main :: IO String
main =
  handleEffects
    program
    ( liftHandler . teletypeIO )
```
