module Network.Stellar.Keypair
    ( KeyPair(..)
    , PublicKey(..)
    , generateKeypair
    , fromPrivateKey
    , fromPrivateKey'
    , signatureHint
    , encodePublic
    , encodePublicKey
    , decodePublic
    , decodePublicKey
    , decodePublic'
    , decodePublicKey'
    , encodePrivate
    , decodePrivate
    , decodePrivate'
    )where

import           Control.Monad (guard)
import           Crypto.Random (getSystemDRG, randomBytesGenerate)
import           Crypto.Sign.Ed25519
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Base32 (decodeBase32, encodeBase32)
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word (Word16, Word8)
import           GHC.Stack (HasCallStack)

data KeyPair = KeyPair
    { kpPublicKey   :: PublicKey
    , kpPrivateKey  :: SecretKey
    , kpSeed        :: ByteString
    }

instance Show KeyPair where
    show (KeyPair public _ seed) =
        "KeyPair {" ++ Text.unpack (encodePublic $ unPublicKey public) ++ ", "
        ++ Text.unpack (encodePrivate seed) ++ "}"

generateKeypair :: IO KeyPair
generateKeypair = do
    gen <- getSystemDRG
    let (randomBytes, _) = randomBytesGenerate 32 gen
    return $ fromSeed randomBytes

fromSeed :: ByteString -> KeyPair
fromSeed seed = KeyPair public private seed
    where (public, private) = fromJust $ createKeypairFromSeed_ seed

fromPrivateKey :: Text -> Maybe KeyPair
fromPrivateKey = fmap fromSeed . decodePrivate

fromPrivateKey' :: HasCallStack => Text -> KeyPair
fromPrivateKey' = fromSeed . decodePrivate'

signatureHint :: KeyPair -> ByteString
signatureHint = BS.drop 28 . unPublicKey . kpPublicKey


encodePublic :: ByteString -> Text
encodePublic = encodeKey EncodingAccount

encodePublicKey :: PublicKey -> Text
encodePublicKey = encodePublic . unPublicKey

encodePrivate :: ByteString -> Text
encodePrivate = encodeKey EncodingSeed

decodePublic :: Text -> Maybe ByteString
decodePublic = decodeKey EncodingAccount

decodePublicKey :: Text -> Maybe PublicKey
decodePublicKey = fmap PublicKey . decodeKey EncodingAccount

decodePublic' :: Text -> ByteString
decodePublic' = decodeKey' EncodingAccount

decodePublicKey' :: Text -> PublicKey
decodePublicKey' = PublicKey . decodePublic'

decodePrivate :: Text -> Maybe ByteString
decodePrivate = decodeKey EncodingSeed

decodePrivate' :: HasCallStack => Text -> ByteString
decodePrivate' = decodeKey' EncodingSeed

decodeKey :: EncodingVersion -> Text -> Maybe ByteString
decodeKey version key = do
    keyBlob <- either (const Nothing) Just $ decodeBase32 $ encodeUtf8 key
    let (payload, checksum) = BS.splitAt (BS.length keyBlob - 2) keyBlob
    (versionByte, keyData) <- BS.uncons payload
    let versionCheck = versionByte == versionByteName version
        checksumCheck = crc16XmodemLE payload == checksum
    guard (versionCheck && checksumCheck)
    pure keyData

decodeKey' :: HasCallStack => EncodingVersion -> Text -> ByteString
decodeKey' version key =
    fromMaybe (error $ "Decoding key failed " ++ Text.unpack key) $
    decodeKey version key

data EncodingVersion = EncodingAccount | EncodingSeed | EncodingPreAuthTx | EncodingSha256Hash

versionByteName :: EncodingVersion -> Word8
versionByteName EncodingAccount    = 48
versionByteName EncodingSeed       = 144
versionByteName EncodingPreAuthTx  = 152
versionByteName EncodingSha256Hash = 184

encodeKey :: EncodingVersion -> ByteString -> Text
encodeKey version key = encodeBase32 $ payload <> checksum
    where
        versionByte = versionByteName version
        payload = versionByte `BS.cons` key
        checksum = crc16XmodemLE payload

crc16XmodemLE :: ByteString -> ByteString
crc16XmodemLE bs = BS.pack [fromIntegral $ checksum .&. 0xFF, fromIntegral $ checksum `shiftR` 8]
    where checksum = BS.foldl crcRound 0 bs

crcRound :: Word16 -> Word8 -> Word16
crcRound crc byte = crc2
    where
        code = (crc `shiftR` 8) `xor` fromIntegral byte
        code2 = code `xor` (code `shiftR` 4)
        crc2 = (crc `shiftL` 8) `xor` code2 `xor` (code2 `shiftL` 5) `xor` (code2 `shiftL` 12)
