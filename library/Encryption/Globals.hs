module Encryption.Globals where

import           Data.Word8
import           Crypto.Number.F2m
import qualified Data.ByteString               as B

data ModeOfOperation = ECB | CBC deriving (Show, Eq)

data KeySize = KS128 | KS192 | KS256 deriving (Show, Eq)

type Byte = Word8

type Block = B.ByteString

type BlockStream = B.ByteString

type Key = B.ByteString

type SubKey = B.ByteString

type InitializationVector = B.ByteString

type RoundCoefficient = B.ByteString

blockSize = 16 :: Int

numRounds :: KeySize -> Int
numRounds KS128 = 10
numRounds KS192 = 12
numRounds KS256 = 14

getKeySize :: KeySize -> Int
getKeySize KS128 = 16
getKeySize KS192 = 24
getKeySize KS256 = 32

aesPolynomial = 0x11B -- x^8+x^4+x^3+x+1 -> 100011011 -> 0x11B

mulPolynomial :: Integer -> Integer -> Integer
mulPolynomial = mulF2m aesPolynomial

sampleKey =
  B.pack
    [ 0x2B
    , 0x7E
    , 0x15
    , 0x16
    , 0x28
    , 0xAE
    , 0xD2
    , 0xA6
    , 0xAB
    , 0xF7
    , 0x15
    , 0x88
    , 0x09
    , 0xCF
    , 0x4F
    , 0x3C
    ] :: Key
    -- 2B7E151628AED2A6ABF7158809CF4F3C

sampleKey192 = B.pack
  [ 0x2B
  , 0x7E
  , 0x15
  , 0x16
  , 0x28
  , 0xae
  , 0xD2
  , 0xA6
  , 0xAb
  , 0xF7
  , 0x15
  , 0x88
  , 0x09
  , 0xCF
  , 0x4F
  , 0x3C
  , 0x2B
  , 0x7E
  , 0x15
  , 0x16
  , 0x28
  , 0xae
  , 0xD2
  , 0xA6
  ] :: Key
  --2B7E151628AED2A6ABF7158809CF4F3C2B7E151628AED2A6

sampleKey256 = B.pack
  [ 0x2B
  , 0x7E
  , 0x15
  , 0x16
  , 0x28
  , 0xae
  , 0xD2
  , 0xA6
  , 0xAb
  , 0xF7
  , 0x15
  , 0x88
  , 0x09
  , 0xCF
  , 0x4F
  , 0x3C
  , 0x2B
  , 0x7E
  , 0x15
  , 0x16
  , 0x28
  , 0xae
  , 0xD2
  , 0xA6
  , 0xAb
  , 0xF7
  , 0x15
  , 0x88
  , 0x09
  , 0xCF
  , 0x4F
  , 0x3C
  ] :: Key
  --2B7E151628AED2A6ABF7158809CF4F3C2B7E151628AED2A6ABF7158809CF4F3C

sampleBlock =
  B.pack
    [ 0x32
    , 0x43
    , 0xF6
    , 0xA8
    , 0x88
    , 0x5A
    , 0x30
    , 0x8D
    , 0x31
    , 0x31
    , 0x98
    , 0xA2
    , 0xE0
    , 0x37
    , 0x07
    , 0x34
    ] :: Block
