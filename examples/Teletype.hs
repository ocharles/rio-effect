{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}

import Control.Category ( (.), id )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Prelude hiding ( (.), id )
import RIO.Effect
import RIO.Effect.TH

data Teletype cfg a where
  GetL :: Teletype cfg String
  PutL :: String -> Teletype cfg ()

genSend ''Teletype

teletypeIO :: EFF Teletype cfg :~> MonadIO
teletypeIO =
  handleEffect $ \case
    GetL -> liftIO getLine
    PutL l -> liftIO ( putStrLn l )

program :: EFF Teletype () m => m String
program = do
  putL "What is your name?"
  getL

main :: IO String
main =
  handleEffects
    program
    ( liftHandler . teletypeIO )
