module Encryption.ShiftRows
  ( shiftRows
  , invShiftRows
  )
where

import qualified Data.ByteString.Lazy               as B
import           Encryption.Utils
import           Encryption.Globals

-- The ShiftRows layer of AES.
shiftRows :: Block -> Block
shiftRows = common rotWordLeft

-- The inverse of the ShiftRows layer of AES.
invShiftRows :: Block -> Block
invShiftRows = common rotWordRight

-- The common part of both shiftRows and invShiftRows.
common :: (Int -> B.ByteString -> B.ByteString) -> Block -> Block
common rotFunc =
  B.concat . B.transpose . zipWith rotFunc [0 .. 3] . B.transpose . splitEvery 4
