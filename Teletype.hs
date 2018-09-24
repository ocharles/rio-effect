{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language TypeOperators #-}

module Teletype ( Teletype, Teletype.getLine, putLine, toIO ) where

import Control.Monad.IO.Class
import Effect


data Teletype cfg a where
  GetLine :: Teletype () String
  PutLine :: String -> Teletype () ()


getLine :: EFF Teletype () m => m String
getLine = send GetLine


putLine :: EFF Teletype () m => String -> m ()
putLine = send . PutLine


toIO :: String -> Teletype cfg :~> MonadIO
toIO s = HandleWith (\case
  GetLine -> liftIO ( Prelude.putStrLn s >> return s )
  PutLine l -> liftIO $ putStrLn l)
