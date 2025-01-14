{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | XDR Serialization
module Network.ONCRPC.XDR.Serial (
  XDR (..),
  XDREnum (..),
  xdrToEnum',
  xdrPutEnum,
  xdrGetEnum,
  XDRUnion (..),
  xdrDiscriminant,
  xdrPutUnion,
  xdrGetUnion,
  xdrSerialize,
  xdrSerializeLazy,
  xdrDeserialize,
  xdrDeserializeLazy,
) where

import Control.Monad (guard, replicateM, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromJust, listToMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Serialize (Get, Put)
import Data.Serialize qualified as Serialize
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.TypeLits (natVal)
import Network.ONCRPC.XDR.Types qualified as XDR

import Network.ONCRPC.XDR.Array

instance MonadFail (Either String) where
  fail = Left

-- | An XDR type that can be (de)serialized.
class XDR a where
  -- | XDR identifier/type descriptor; argument value is ignored.
  xdrType :: a -> String

  xdrPut :: a -> Put

  xdrGet :: Get a

instance XDR XDR.Int where
  xdrType _ = "int"
  xdrPut = Serialize.putInt32be
  xdrGet = Serialize.getInt32be

instance XDR XDR.UnsignedInt where
  xdrType _ = "unsigned int"
  xdrPut = Serialize.putWord32be
  xdrGet = Serialize.getWord32be

instance XDR XDR.Hyper where
  xdrType _ = "hyper"
  xdrPut = Serialize.putInt64be
  xdrGet = Serialize.getInt64be

instance XDR XDR.UnsignedHyper where
  xdrType _ = "unsigned hyper"
  xdrPut = Serialize.putWord64be
  xdrGet = Serialize.getWord64be

instance XDR XDR.Float where
  xdrType _ = "float"
  xdrPut = Serialize.putFloat32be
  xdrGet = Serialize.getFloat32be

instance XDR XDR.Double where
  xdrType _ = "double"
  xdrPut = Serialize.putFloat64be
  xdrGet = Serialize.getFloat64be

instance XDR XDR.Bool where
  xdrType _ = "bool"
  xdrPut = xdrPutEnum
  xdrGet = xdrGetEnum

{- | An XDR type defined with \"enum\".
 Note that the 'XDREnum' 'XDR.Int' value is not (necessarily) the same as the 'Enum' 'Int' value.
 The 'Enum' instance is derived automatically to allow 'succ', etc. to work usefully in Haskell, whereas the 'XDREnum' reflects the XDR-defined values.
-}
class (XDR a, Enum a) => XDREnum a where
  xdrFromEnum :: a -> XDR.Int
  xdrToEnum :: (MonadFail m) => XDR.Int -> m a

instance XDREnum XDR.Int where
  xdrFromEnum = id
  xdrToEnum = pure

instance XDREnum XDR.UnsignedInt where
  xdrFromEnum = fromIntegral
  xdrToEnum = pure . fromIntegral

-- | Version of 'xdrToEnum' that fails at runtime for invalid values: @fromMaybe undefined . 'xdrToEnum'@.
xdrToEnum' :: (XDREnum a) => XDR.Int -> a
xdrToEnum' = either error id . xdrToEnum

-- | Default implementation of 'xdrPut' for 'XDREnum'.
xdrPutEnum :: (XDREnum a) => a -> Put
xdrPutEnum = Serialize.put . xdrFromEnum

-- | Default implementation of 'xdrGet' for 'XDREnum'.
xdrGetEnum :: (XDREnum a) => Get a
xdrGetEnum = xdrToEnum =<< Serialize.get

instance XDREnum XDR.Bool where
  xdrFromEnum False = 0
  xdrFromEnum True = 1
  xdrToEnum 0 = pure False
  xdrToEnum 1 = pure True
  xdrToEnum _ = fail "invalid bool"

-- | An XDR type defined with \"union\"
class (XDR a, XDREnum (XDRDiscriminant a)) => XDRUnion a where
  type XDRDiscriminant a

  -- | Split a union into its discriminant and body generator.
  xdrSplitUnion :: a -> (XDR.Int, Put)

  -- | Get the body of a union based on its discriminant.
  xdrGetUnionArm :: XDR.Int -> Get a

xdrDiscriminant :: (XDRUnion a) => a -> XDRDiscriminant a
xdrDiscriminant = xdrToEnum' . fst . xdrSplitUnion

-- | Default implementation of 'xdrPut' for 'XDRUnion'.
xdrPutUnion :: (XDRUnion a) => a -> Put
xdrPutUnion = uncurry ((>>) . xdrPut) . xdrSplitUnion

-- | Default implementation of 'xdrGet' for 'XDRUnion'.
xdrGetUnion :: (XDRUnion a) => Get a
xdrGetUnion = xdrGet >>= xdrGetUnionArm

instance (XDR a) => XDR (XDR.Optional a) where
  xdrType = ('*' :) . xdrType . fromJust
  xdrPut = xdrPutUnion
  xdrGet = xdrGetUnion

instance (XDR a) => XDRUnion (XDR.Optional a) where
  type XDRDiscriminant (XDR.Optional a) = XDR.Bool
  xdrSplitUnion Nothing = (0, pure ())
  xdrSplitUnion (Just a) = (1, xdrPut a)
  xdrGetUnionArm 0 = pure Nothing
  xdrGetUnionArm 1 = Just <$> xdrGet
  xdrGetUnionArm _ = fail $ "xdrGetUnion: invalid discriminant for " ++ xdrType (undefined :: XDR.Optional a)

xdrPutPad :: XDR.Length -> Put
xdrPutPad n = case n `mod` 4 of
  0 -> pure ()
  1 -> Serialize.putWord16host 0 >> Serialize.putWord8 0
  2 -> Serialize.putWord16host 0
  _ {- must be 3 -} -> Serialize.putWord8 0

xdrGetPad :: XDR.Length -> Get ()
xdrGetPad n = case n `mod` 4 of
  0 -> pure ()
  1 -> do
    0 <- Serialize.getWord16host
    0 <- Serialize.getWord8
    pure ()
  2 -> do
    0 <- Serialize.getWord16host
    pure ()
  _ {- must be 3 -} -> do
    0 <- Serialize.getWord8
    pure ()

bsLength :: BS.ByteString -> XDR.Length
bsLength = fromIntegral . BS.length

xdrPutByteString :: XDR.Length -> BS.ByteString -> Put
xdrPutByteString l b = do
  unless (bsLength b == l) $ error "xdrPutByteString: incorrect length"
  Serialize.putByteString b
  xdrPutPad l

xdrGetByteString :: XDR.Length -> Get BS.ByteString
xdrGetByteString l = do
  b <- Serialize.getByteString $ fromIntegral l
  xdrGetPad l
  pure b

fixedLength :: forall n a. (KnownNat n) => LengthArray 'EQ n a -> String -> String
fixedLength a = (++ ('[' : show (fixedLengthArrayLength a) ++ "]"))

variableLength :: forall n a. (KnownNat n) => LengthArray 'LT n a -> String -> String
variableLength a
  | n == XDR.maxLength = (++ "<>")
  | otherwise = (++ ('<' : show n ++ ">"))
 where
  n = fromIntegral $ boundedLengthArrayBound a

xdrGetBoundedArray :: forall n a. (KnownNat n) => (XDR.Length -> Get a) -> Get (LengthArray 'LT n a)
xdrGetBoundedArray g = do
  l <- xdrGet
  guard $ l <= fromIntegral (boundedLengthArrayBound (undefined :: LengthArray 'LT n a))
  unsafeLengthArray <$> g l

instance (KnownNat n, XDR a) => XDR (LengthArray 'EQ n [a]) where
  xdrType la =
    fixedLength la $ xdrType $ fromJust $ listToMaybe $ unLengthArray la
  xdrPut la = mapM_ xdrPut a where a = unLengthArray la
  xdrGet =
    unsafeLengthArray <$> replicateM (fromInteger $ natVal $ Proxy @n) xdrGet

instance (KnownNat n, XDR a) => XDR (LengthArray 'LT n [a]) where
  xdrType la =
    variableLength la $ xdrType $ fromJust $ listToMaybe $ unLengthArray la
  xdrPut la = do
    xdrPut (fromIntegral (length a) :: XDR.Length)
    mapM_ xdrPut a
   where
    a = unLengthArray la
  xdrGet = xdrGetBoundedArray $ \l -> replicateM (fromIntegral l) xdrGet

instance (KnownNat n, XDR a) => XDR (LengthArray 'EQ n (Vector a)) where
  xdrType la = fixedLength la $ xdrType $ Vector.head $ unLengthArray la
  xdrPut la = mapM_ xdrPut a where a = unLengthArray la
  xdrGet =
    unsafeLengthArray
      <$> Vector.replicateM (fromInteger $ natVal $ Proxy @n) xdrGet

instance (KnownNat n, XDR a) => XDR (LengthArray 'LT n (Vector a)) where
  xdrType la = variableLength la $ xdrType $ Vector.head $ unLengthArray la
  xdrPut la = do
    xdrPut (fromIntegral (length a) :: XDR.Length)
    mapM_ xdrPut a
   where
    a = unLengthArray la
  xdrGet = xdrGetBoundedArray $ \l -> Vector.replicateM (fromIntegral l) xdrGet

instance (KnownNat n) => XDR (LengthArray 'EQ n BS.ByteString) where
  xdrType o = fixedLength o "opaque"
  xdrPut o =
    xdrPutByteString (fromInteger $ natVal $ Proxy @n) $ unLengthArray o
  xdrGet =
    unsafeLengthArray <$> xdrGetByteString (fromInteger $ natVal $ Proxy @n)

instance (KnownNat n) => XDR (LengthArray 'LT n BS.ByteString) where
  xdrType o = variableLength o "opaque"
  xdrPut o = do
    xdrPut l
    xdrPutByteString l b
   where
    l = bsLength b
    b = unLengthArray o
  xdrGet = xdrGetBoundedArray xdrGetByteString

instance XDR () where
  xdrType () = "void"
  xdrPut () = pure ()
  xdrGet = pure ()

instance (XDR a, XDR b) => XDR (a, b) where
  xdrType (a, b) = xdrType a ++ '+' : xdrType b
  xdrPut (a, b) = xdrPut a >> xdrPut b
  xdrGet = (,) <$> xdrGet <*> xdrGet

instance (XDR a, XDR b, XDR c) => XDR (a, b, c) where
  xdrType (a, b, c) = xdrType a ++ '+' : xdrType b ++ '+' : xdrType c
  xdrPut (a, b, c) = xdrPut a >> xdrPut b >> xdrPut c
  xdrGet = (,,) <$> xdrGet <*> xdrGet <*> xdrGet

instance (XDR a, XDR b, XDR c, XDR d) => XDR (a, b, c, d) where
  xdrType (a, b, c, d) = xdrType a ++ '+' : xdrType b ++ '+' : xdrType c ++ '+' : xdrType d
  xdrPut (a, b, c, d) = xdrPut a >> xdrPut b >> xdrPut c >> xdrPut d
  xdrGet = (,,,) <$> xdrGet <*> xdrGet <*> xdrGet <*> xdrGet

xdrSerialize :: (XDR a) => a -> BS.ByteString
xdrSerialize = Serialize.runPut . xdrPut

xdrSerializeLazy :: (XDR a) => a -> BSL.ByteString
xdrSerializeLazy = Serialize.runPutLazy . xdrPut

-- | @"S.runGet' 'xdrGet'@
xdrDeserialize :: (XDR a) => BS.ByteString -> Either String a
xdrDeserialize = Serialize.runGet xdrGet

-- | @"S.runGetLazy' 'xdrGet'@
xdrDeserializeLazy :: (XDR a) => BSL.ByteString -> Either String a
xdrDeserializeLazy = Serialize.runGetLazy xdrGet
