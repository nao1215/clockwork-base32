{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           ClockworkBase32 (decode, decodeString, encode, encodeString)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  -- testcase from
  -- https://github.com/szktty/go-clockwork-base32/blob/c2cac4daa7ad2045089b943b377b12ac57e3254e/base32_test.go#L36-L44
  -- https://github.com/shiguredo/erlang-base32/blob/0cc88a702ce1d8ca345e516a05a9a85f7f23a718/test/base32_clockwork_test.erl#L7-L18
  describe "encode" $ do
    it "returns '' when input value is ''" $ do
      encode "" `shouldBe` ""

    it "returns 'CR' when input value is 'f'" $ do
      encode "f" `shouldBe` "CR"

    it "returns 'CSQG' when input value is 'fo'" $ do
      encode "fo" `shouldBe` "CSQG"

    it "returns 'CSQPY' when input value is 'foo'" $ do
      encode "foo" `shouldBe` "CSQPY"

    it "returns 'CSQPYRG' when input value is 'foob'" $ do
      encode "foob" `shouldBe` "CSQPYRG"

    it "returns 'CSQPYRK1' when input value is 'fooba'" $ do
      encode "fooba" `shouldBe` "CSQPYRK1"

    it "returns 'CSQPYRK1E8' when input value is 'foobar'" $ do
      encode "foobar" `shouldBe` "CSQPYRK1E8"

    it "returns '91JPRV3F5GG7EVVJDHJ22' when input value is 'Hello, world!'" $ do
      encode "Hello, world!" `shouldBe` "91JPRV3F5GG7EVVJDHJ22"

    it "returns 'AHM6A83HENMP6TS0C9S6YXVE41K6YY10D9TPTW3K41QQCSBJ41T6GS90DHGQMY90CHQPEBG' when input value is 'The quick brown fox jumps over the lazy dog.'" $ do
      encode "The quick brown fox jumps over the lazy dog." `shouldBe` "AHM6A83HENMP6TS0C9S6YXVE41K6YY10D9TPTW3K41QQCSBJ41T6GS90DHGQMY90CHQPEBG"

    it "returns '07EKWRQY2N7DEAVD5MJ3JX36KM' when input value is '\x01\xdd\x3e\x62\xfe\x15\x4e\xd7\x2b\x6d\x2d\x24\x39\x74\x66\x9d'" $ do
      encode "\x01\xdd\x3e\x62\xfe\x15\x4e\xd7\x2b\x6d\x2d\x24\x39\x74\x66\x9d" `shouldBe` "07EKWRQY2N7DEAVD5MJ3JX36KM"

    it "returns 'AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44' when input value is 'Wow, it really works!'" $ do
      encode "Wow, it really works!" `shouldBe` "AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44"

  describe "encodeString" $ do
    it "returns 'AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44' when input value is 'Wow, it really works!'" $ do
      encodeString "Wow, it really works!" `shouldBe` "AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44"

  describe "decode" $ do
    it "returns Right '' when input value is ''" $ do
      decode "" `shouldBe` Right ""

    it "returns Right 'f' when input value is 'CR'" $ do
      decode "CR" `shouldBe` Right "f"

    it "returns Right 'fo' when input value is 'CSQG'" $ do
      decode "CSQG" `shouldBe` Right "fo"

    it "returns Right 'foo' when input value is 'CSQPY'" $ do
      decode "CSQPY" `shouldBe` Right "foo"

    it "returns Right 'foob' when input value is 'CSQPYRG'" $ do
      decode "CSQPYRG" `shouldBe` Right "foob"

    it "returns Right 'fooba' when input value is 'CSQPYRK1'" $ do
      decode "CSQPYRK1" `shouldBe` Right "fooba"

    it "returns Right 'foobar' when input value is 'CSQPYRK1E8'" $ do
      decode "CSQPYRK1E8" `shouldBe` Right "foobar"

    it "returns Right 'Hello, world!' when input value is '91JPRV3F5GG7EVVJDHJ22'" $ do
      decode "91JPRV3F5GG7EVVJDHJ22" `shouldBe` Right "Hello, world!"

    it "returns Right 'The quick brown fox jumps over the lazy dog.' when input value is 'AHM6A83HENMP6TS0C9S6YXVE41K6YY10D9TPTW3K41QQCSBJ41T6GS90DHGQMY90CHQPEBG'" $ do
      decode "AHM6A83HENMP6TS0C9S6YXVE41K6YY10D9TPTW3K41QQCSBJ41T6GS90DHGQMY90CHQPEBG" `shouldBe` Right "The quick brown fox jumps over the lazy dog."

    it "returns Right '\x01\xdd\x3e\x62\xfe\x15\x4e\xd7\x2b\x6d\x2d\x24\x39\x74\x66\x9d' when input value is '07EKWRQY2N7DEAVD5MJ3JX36KM'" $ do
      decode "07EKWRQY2N7DEAVD5MJ3JX36KM" `shouldBe` Right "\x01\xdd\x3e\x62\xfe\x15\x4e\xd7\x2b\x6d\x2d\x24\x39\x74\x66\x9d"

    it "returns Right 'Wow, it really works!' when input value is 'AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44'" $ do
      decode "AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44" `shouldBe` Right "Wow, it really works!"

    it "also accepts lowercase letters" $ do
      decode "axqqeb10d5t20wk5c5p6ry90exqq4tvk44" `shouldBe` decode "AXQQEB10D5T20WK5C5P6RY90EXQQ4TVK44"

    it "also accepts o and O instead of 0" $ do
      decode "ooOO00" `shouldBe` decode "000000"

    it "also accepts i, I, l and L instead of 1" $ do
      decode "iIlL1111" `shouldBe` decode "11111111"

    it "returns Left 'Invalid character: ~' when input value is '~'" $ do
      decode "~" `shouldBe` Left "Invalid character: ~"

  describe "decodeString" $ do
    it "returns Right 'foobar' when input value is 'CSQPYRK1E8'" $ do
      decodeString "CSQPYRK1E8" `shouldBe` Right "foobar"
