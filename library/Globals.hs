module Globals where

import           Data.Word8
import           Crypto.Number.F2m
import qualified Data.ByteString               as B

data ModeOfOperation = ECB | CBC | CTR deriving (Show, Eq)

type Byte = Word8

type Block = B.ByteString

type Key = B.ByteString

type SubKey = B.ByteString

type RoundCoefficient = B.ByteString

blockSize = 16 :: Int

numRounds = 10 :: Int

aesPolynomial = 0x11B -- x^8+x^4+x^3+x+1 -> 100011011 -> 0x11B

mulPolynomial :: Integer -> Integer -> Integer
mulPolynomial = mulF2m aesPolynomial

sampleKey :: Key
sampleKey = B.pack
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
  ]
