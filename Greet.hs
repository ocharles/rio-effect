{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language TypeOperators #-}

module Greet ( Greet, greet, Greet.toIO, toTeletype ) where

import Control.Monad.IO.Class
import Effect
import Teletype


data Greet cfg a where
  Greet :: String -> Greet () ()


greet :: EFF Greet () m => String -> m ()
greet = send . Greet


toIO :: Greet cfg :~> AllOf '[ MonadIO ]
toIO = HandleWith (\case
  Greet who -> liftIO ( putStrLn ( "Hi, " ++ who ++ "!" ) ))


toTeletype :: Greet cfg :~> EFF Teletype ()
toTeletype = HandleWith (\case
  Greet who -> Teletype.putLine ( "Hi, " ++ who ++ "!" ))
