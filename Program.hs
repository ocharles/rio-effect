{-# language FlexibleContexts #-}

module Program where

import Effect
import Greet
import Teletype

program :: ( EFF Greet () m, EFF Teletype () m ) => m ()
program =
  Teletype.getLine >>= greet
{-# NOINLINE program #-}
