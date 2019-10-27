module EncDec
  ( encrypt
  , decrypt
  )
where

import           ShiftRows
import           AddRoundKey
import           MixColumns
import           SBox
import           Utils
import           Globals
import           KeySchedule

encrypt :: Key -> Block -> Block
encrypt key block = helper (genSubKeys key) (addRoundKey key block)
 where
  helper :: [SubKey] -> Block -> Block
  helper subKeys
    | length subKeys == 1
    = addRoundKey (head subKeys) . shiftRows . subBytes
    | otherwise
    = helper (tail subKeys)
      . addRoundKey (head subKeys)
      . mixColumns
      . shiftRows
      . subBytes


decrypt :: Key -> Block -> Block
decrypt = undefined
-- encRound :: Key -> Block -> Block

-- encRound key = addRoundKey key . mixColumns . shiftRows . subBytes
