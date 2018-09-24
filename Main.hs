{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Effect
import Greet
import Teletype
import Control.Monad.Trans.Reader
import GHC.Generics
import Program


data Env = Env { greet :: Greet () :~> MonadIO
               , teletype2 :: Teletype () :~> MonadIO }
  deriving ( Generic, Handles Teletype, Handles Greet )


data Env2 = Env2 (Teletype () :~> MonadIO) Bool
  deriving ( Generic, Handles Teletype )


main :: IO ()
main = do
  x <- fmap Teletype.toIO Prelude.getLine
  runReaderT program ( Greet.toTeletype, "Hello", x )
