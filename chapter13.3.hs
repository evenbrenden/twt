#! /usr/bin/env nix-shell
#! nix-shell chapter13.3.nix -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Monad.Writer
import           Data.Aeson                     ( (.=)
                                                , Value(..)
                                                , object
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.ByteString.Lazy.Char8    as LC8
import           Data.Kind                      ( Type )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Typeable
import           Data.Vector                    ( fromList )
import           GHC.Generics
import           GHC.TypeLits
import qualified GHC.TypeLits                  as Err

data Person = Person
    { name        :: String
    , age         :: Int
    , phone       :: Maybe String
    , permissions :: [Bool]
    }
    deriving Generic

class GSchema (a :: Type -> Type) where
    gschema :: Writer [Text] Value

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b
mergeObjects _          _          = error "Not supported"

emitRequired :: forall nm . KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm

type family ToJSONType (a :: Type) :: Symbol where
    ToJSONType Int = "integer"
    ToJSONType Integer = "integer"
    ToJSONType Float = "number"
    ToJSONType Double = "number"
    ToJSONType String = "string"
    ToJSONType Bool = "boolean"
    ToJSONType [a] = "array"
    ToJSONType a = TypeName a

type family RepName (x :: Type -> Type) :: Symbol where
    RepName (D1 ('MetaData nm _ _ _) _) = nm

type family TypeName (t :: Type) :: Symbol where
    TypeName t = RepName (Rep t)

makeTypeObj :: forall a . KnownSymbol (ToJSONType a) => Value
makeTypeObj =
    object ["type" .= String (pack . symbolVal $ Proxy @(ToJSONType a))]

makePropertyObj :: forall name . (KnownSymbol name) => Value -> Value
makePropertyObj v = object [pack (symbolVal $ Proxy @name) .= v]

instance (KnownSymbol nm, KnownSymbol (ToJSONType a))
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 a)) where
    gschema = do
        emitRequired @nm
        pure . makePropertyObj @nm $ makeTypeObj @a
    {-# INLINE gschema #-}

instance (GSchema f, GSchema g) => GSchema (f :*: g) where
    gschema = mergeObjects <$> gschema @f <*> gschema @g
    {-# INLINE gschema #-}

instance (TypeError ('Err.Text "JSON Schema does not support sum types"))
        => GSchema (f :+: g) where
    gschema = error "JSON Schema does not support sum types"
    {-# INLINE gschema #-}

instance GSchema a => GSchema (M1 C _1 a) where
    gschema = gschema @a
    {-# INLINE gschema #-}

instance (GSchema a, KnownSymbol nm)
    => GSchema (M1 D ('MetaData nm _1 _2 _3) a) where
    gschema = do
        sch <- gschema @a
        pure $ object
            [ "title" .= (String . pack . symbolVal $ Proxy @nm)
            , "type" .= String "object"
            , "properties" .= sch
            ]
    {-# INLINE gschema #-}

schema :: forall a . (Generic a, GSchema (Rep a)) => Value
schema =
    let (v, reqs) = runWriter $ gschema @(Rep a)
    in  mergeObjects v
            $ object ["required" .= Array (fromList $ String <$> reqs)]
{-# INLINE schema #-}

instance {-# OVERLAPPING #-}
    ( KnownSymbol nm
    , KnownSymbol (ToJSONType a)
    )
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 (Maybe a))) where
    gschema = pure . makePropertyObj @nm $ makeTypeObj @a
    {-# INLINE gschema #-}

instance {-# OVERLAPPING #-}
    ( KnownSymbol nm
    , KnownSymbol (ToJSONType [a])
    , KnownSymbol (ToJSONType a)
    )
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 [a])) where
    gschema = do
        emitRequired @nm
        let innerType = object ["items" .= makeTypeObj @a]
        pure . makePropertyObj @nm . mergeObjects innerType $ makeTypeObj @[a]
    {-# INLINE gschema #-}

instance {-# OVERLAPPING #-}
    KnownSymbol nm
     => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 String)) where
    gschema = do
        emitRequired @nm
        pure . makePropertyObj @nm $ makeTypeObj @String
    {-# INLINE gschema #-}

personSchema :: LC8.ByteString
personSchema = encodePretty $ schema @Person

-- > LC8.putStrLn personSchema
-- {
--     "required": [
--         "name",
--         "age",
--         "permissions"
--     ],
--     "title": "Person",
--     "type": "object",
--     "properties": {
--         "phone": {
--             "type": "string"
--         },
--         "age": {
--             "type": "integer"
--         },
--         "name": {
--             "type": "string"
--         },
--         "permissions": {
--             "items": {
--                 "type": "boolean"
--             },
--             "type": "array"
--         }
--     }
-- }
