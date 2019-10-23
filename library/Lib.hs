module Lib where

import           Data.Word8
import           Data.Bits

data ModeOfOperation = ECB | CBC | CTR deriving (Show, Eq)

type Byte = Word8

type Block = [Byte]

type Key = [Byte]

blockSize = 16 :: Num a => a

numRounds = 10 :: Num a => a