module KeySchedule where

import           Data.Word8
import           Data.Bits
import           Lib

g :: [Byte] -> Byte -> [Byte]

g (x:y:xs) rc = xor y rc : xs ++ [x]