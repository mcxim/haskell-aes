module KeySchedule where

import           Data.Word8
import           Data.Bits
import qualified Data.ByteString               as B
import           Lib

g :: B.ByteString -> Byte -> B.ByteString
g bs rc = (xor y rc `B.cons` xs) `B.snoc` y
 where
  x  = B.head bs
  y  = B.head . B.tail $ bs
  xs = B.tail . B.tail $ bs