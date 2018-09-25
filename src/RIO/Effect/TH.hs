{-# language TemplateHaskell #-}

module RIO.Effect.TH ( genSend ) where

-- base
import Data.Char ( toLower )

-- rio
import RIO
import RIO.Effect

-- template-haskell
import qualified Language.Haskell.TH as TH


genSend :: TH.Name -> TH.DecsQ
genSend tyCon = do
  info <-
    TH.reify tyCon

  case info of
    TH.TyConI ( TH.DataD _cxt effectName _tyvars _kind cons _deriv ) ->
      fmap ( concat . concat ) $ for cons $ \con ->
        case con of
          TH.ForallC vars _ ( TH.GadtC names args ( _ `TH.AppT` cfg `TH.AppT` ret ) ) ->
            for names $ \n -> do
              opName <-
                TH.mkName <$> mkOpName ( TH.nameBase n )

              m <-
                TH.newName "m"

              argNames <-
                traverse ( \_ -> TH.newName "x" ) args

              return
                [ TH.SigD
                    opName
                    ( TH.ForallT
                        ( TH.PlainTV m
                        : case cfg of
                            TH.VarT cfgName ->
                              [ TH.PlainTV cfgName ]

                            _ ->
                              []
                        )
                        [ TH.ConT ''EFF
                            `TH.AppT` TH.ConT effectName
                            `TH.AppT` cfg
                            `TH.AppT` TH.VarT m
                        ]
                        ( foldr
                            ( TH.AppT . TH.AppT TH.ArrowT )
                            ( TH.VarT m `TH.AppT` ret )
                            ( map
                                ( \( _bang, t ) ->
                                    t
                                )
                                args
                            )
                        )
                    )
                , TH.FunD
                    opName
                    [ TH.Clause
                        ( map TH.VarP argNames )
                        ( TH.NormalB
                            ( TH.VarE 'send
                                `TH.AppE`
                                  ( foldl
                                      TH.AppE
                                      ( TH.ConE n )
                                      ( map TH.VarE argNames )
                                  )
                            )
                        )
                        []
                    ]
                ]

          o ->
            fail ( "Must be a GADT: " ++ show o )

    o ->
      fail ( "genSend must be called with a type constructor: " ++ show o )


-- | Pick a name for an operation.
-- For normal constructors it lowers first letter.
-- For infix ones it omits the first @:@.
mkOpName :: String -> TH.Q String
mkOpName (':':name) = return name
mkOpName ( c :name) = return $ toLower c : name
mkOpName _ = fail "impossible happened: empty (null) constructor name"
