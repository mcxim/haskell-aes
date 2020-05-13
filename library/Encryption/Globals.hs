module Encryption.Globals where

import           Data.Word8
import qualified Data.ByteString.Lazy               as B
import GHC.Int (Int64)

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

getKeySize :: KeySize -> Int64
getKeySize KS128 = 16
getKeySize KS192 = 24
getKeySize KS256 = 32

aesPolynomial = 0x11B -- x^8+x^4+x^3+x+1 -> 100011011 -> 0x11B
