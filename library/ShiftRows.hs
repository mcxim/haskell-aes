module ShiftRows
  ( shiftRows
  )
where

import qualified Data.ByteString               as B
import           Utils
import           Globals

shiftRows :: Block -> Block
shiftRows block = B.concat . B.transpose $ zipWith
  rotWordLeft
  [0 .. 3]
  (B.transpose $ splitEvery 4 block)
