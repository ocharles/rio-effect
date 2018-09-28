{-# language FunctionalDependencies #-}
{-# language PolyKinds #-}

module RIO.Effect.EFF ( EFF ) where

class EFF ( eff :: k -> * -> * ) ( cfg :: k ) ( m :: * -> * ) | eff m -> cfg
