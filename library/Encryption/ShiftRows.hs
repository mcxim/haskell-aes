module ShiftRows
  ( shiftRows
  , invShiftRows
  )
where

import qualified Data.ByteString               as B
import           Utils
import           Globals

shiftRows :: Block -> Block
shiftRows = common rotWordLeft

invShiftRows :: Block -> Block
invShiftRows = common rotWordRight

common :: (Int -> B.ByteString -> B.ByteString) -> Block -> Block
common rotFunc =
  B.concat . B.transpose . zipWith rotFunc [0 .. 3] . B.transpose . splitEvery 4
