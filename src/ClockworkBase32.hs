module ClockworkBase32
  ( encode,
    encodeString,
    decode,
    decodeString
  ) where

import           Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Word             (Word8)
import           Data.Char             (toUpper)

-- clockwork base32 symbols
base32Symbols :: String
base32Symbols = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

-- | Encode a string into Clockwork Base32 format.
-- This function encodes a given input string into the Clockwork Base32 encoding
-- by processing it in chunks of 5 bytes. The input is converted into a sequence
-- of Base32 symbols using the Clockwork Base32 alphabet.
--
-- For example:
-- encodeString "foobar" => "CSQPYRK1E8"
encodeString :: String -> String
encodeString = encode . BSC.pack

-- | Encode a binary sequence into CLockwork Base32 format.
encode :: ByteString -> String
encode input = processChunks inputBytes
  where
    inputBytes = BS.unpack input
    processChunks [] = ""
    processChunks bytes = encodeChunk (take 5 bytes) ++ processChunks (drop 5 bytes)

-- | Encode a chunk of 5 bytes into Base32 symbols.
-- This function takes a chunk of 1 to 5 bytes and converts it into the corresponding
-- Base32 symbols. If the chunk is smaller than 5 bytes, the number of output symbols
-- will be adjusted accordingly.
--
-- For example:
-- encodeChunk [102] => "CR"
encodeChunk :: [Word8] -> String
encodeChunk chunk = take outputLength $ map (base32Symbols !!) indices
  where
    -- 5 bytes -> 40 bits
    val :: Integer
    val = foldl (\acc (b, i) -> acc .|. (fromIntegral b `shiftL` (32 - 8 * i))) 0 (zip chunk [0..])
    -- split 40 bits into 8 5-bit indices
    indices = [fromIntegral ((val `shiftR` (35 - 5 * i)) .&. 0x1F) | i <- [0..7]]
    -- determine the number of symbols to output based on the input size
    outputLength = case length chunk of
      1 -> 2
      2 -> 4
      3 -> 5
      4 -> 7
      5 -> 8
      _ -> 0

-- | Decode a Clockwork Base32 encoded string into the original string.
decodeString :: String -> Either String String
decodeString input = BSC.unpack <$> decode input

-- | Decode a Clockwork Base32 encoded string into the original binary sequence.
decode :: String -> Either String ByteString
decode input = case mapM decodeChar input of
  Left err      -> Left err
  Right symbols -> processChunks symbols []
  where
    -- convert a Base32 symbol into a value
    decodeChar :: Char -> Either String Word8
    decodeChar c = case lookup (toUpper c) decodeMap of
      Just val -> Right val
      Nothing 
        | c `elem` "oO" -> decodeChar '0'
        | c `elem` "iIlL" -> decodeChar '1'
        | otherwise -> Left ("Invalid character: " ++ [c])

    -- process the input symbols in chunks of 8 characters
    processChunks :: [Word8] -> [Word8] -> Either String ByteString
    processChunks [] acc = Right (BS.pack (reverse acc))
    processChunks symbols acc = case decodeChunk (take 8 symbols) of
      []    -> processChunks (drop 8 symbols) acc
      chunk -> processChunks (drop 8 symbols) (reverse chunk ++ acc)

    -- decode a chunk of 8 Base32 symbols into 5 bytes
    decodeChunk :: [Word8] -> [Word8]
    decodeChunk chunk = [fromIntegral byte | byte <- [v1, v2, v3, v4, v5], byte /= 0]
      where
        -- 8 symbols -> 40 bits
        val :: Integer
        val = foldl (\acc (b, i) -> acc .|. (fromIntegral b `shiftL` (35 - 5 * i))) 0 (zip chunk [0..])
        -- split 40 bits into 5 8-bit values
        v1 = (val `shiftR` 32) .&. 0xFF
        v2 = (val `shiftR` 24) .&. 0xFF
        v3 = (val `shiftR` 16) .&. 0xFF
        v4 = (val `shiftR` 8) .&. 0xFF
        v5 = val .&. 0xFF

    -- create a map for decoding Base32 symbols
    decodeMap :: [(Char, Word8)]
    decodeMap = zip base32Symbols [0..]
