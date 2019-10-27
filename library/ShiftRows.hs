module ShiftRows
  ( shiftRows
  )
where

import qualified Data.ByteString               as B
import           Utils
import           Globals

shiftRows :: Block -> Block
shiftRows block = B.concat $ zipWith rotWordLeft [0 .. 3] (splitEvery 4 block)
