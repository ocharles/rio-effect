{-# language NoMonomorphismRestriction #-}
{-# language RankNTypes #-}

import Control.Monad.IO.Class

newtype Constrained c m a = Constrained ( c => m () )

foo =
  Constrained ( liftIO ( putStrLn "Hello" ) )
