{-# LANGUAGE StandaloneDeriving,DeriveGeneric #-}
module Data.Serialize.Instances where

import Data.Hashable
import Data.HashMap.Lazy as Lazy
import Data.List.NonEmpty
import Data.Serialize
import Data.Typeable
import Data.Typeable.Internal

import GHC.Generics

class Serialize1 f where
    put1 :: Serialize a => Putter (f a)
    default put1 :: Serialize (f a) => Putter (f a)
    put1 = put
    get1 :: Serialize a => Get (f a)
    default get1 :: Serialize (f a) => Get (f a)
    get1 = get

deriving instance Generic Fingerprint
deriving instance Generic TypeRep

instance Serialize Fingerprint where
instance Serialize TyCon where
    get = mkTyCon3 <$> get <*> get <*> get
    put x = do
        put $ tyConPackage x
        put $ tyConModule x
        put $ tyConName x
instance Serialize TypeRep where

instance Serialize1 NonEmpty where

instance Serialize a => Serialize (NonEmpty a) where

instance (Eq k,Hashable k,Serialize k,Serialize a) => Serialize (Lazy.HashMap k a) where
    get = Lazy.fromList <$> get
    put = put . Lazy.toList

