module Globals where

import           Data.Word8
import           Data.Bits
import           Crypto.Number.F2m
import qualified Data.ByteString               as B

data ModeOfOperation = ECB | CBC | CTR deriving (Show, Eq)

type Byte = Word8

type Block = B.ByteString

type Key = B.ByteString

type SubKey = B.ByteString

blockSize = 16 :: Int 

numRounds = 10 :: Int

aesPolynomial = 0x11B  -- x^8+x^4+x^3+x+1 -> 100011011 -> 0x11B

mulPolynomial :: Integer -> Integer -> Integer
mulPolynomial = mulF2m aesPolynomial
