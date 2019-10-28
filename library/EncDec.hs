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

decrypt :: Key -> Block -> Block
decrypt key = helper (reverse $ genSubKeys key) 
 where
  subKeys' = reverse $ genSubKeys key
  helper :: [SubKey] -> Block -> Block
  helper subKeys
    | length subKeys == 1
    = addRoundKey (head subKeys) . invSubBytes . invShiftRows
    | length subKeys == 11
    = helper (tail subKeys) . addRoundKey (head subKeys)
    | otherwise
    = helper (tail subKeys)
      . invMixColumns
      . addRoundKey (head subKeys)
      . invSubBytes
      . invShiftRows

encrypt :: Key -> Block -> Block
encrypt key = helper (genSubKeys key)
 where
  helper :: [SubKey] -> Block -> Block
  helper subKeys
    | length subKeys == 1
    = addRoundKey (head subKeys) . shiftRows . subBytes
    | length subKeys == 11
    = helper (tail subKeys) . addRoundKey (head subKeys)
    | otherwise
    = helper (tail subKeys)
      . addRoundKey (head subKeys)
      . mixColumns
      . shiftRows
      . subBytes


-- encRound key = addRoundKey key . mixColumns . shiftRows . subBytes

