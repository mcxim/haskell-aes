module EncDec where

import           ShiftRows
import           AddRoundKey
import           MixColumns
import           SBox
import           Globals
import           Utils
import           KeySchedule
import qualified Data.ByteString               as B
import qualified Data.ByteString.Conversion    as BC
import qualified Data.Word8                    as W
import           Data.Char                      ( ord )

encryptStream
  :: ModeOfOperation
  -> InitializationVector
  -> Key
  -> BlockStream
  -> BlockStream
encryptStream modeOfOperation iv key blocks
  | modeOfOperation == ECB
  = B.concat . map (encrypt (padKeyIV key)) . splitEvery 16 . pad $ blocks
  | modeOfOperation == CBC
  = B.concat
    $ cbcEncHelper (padKeyIV iv) (padKeyIV key) (splitEvery 16 (pad blocks))
  | modeOfOperation == CTR
  = undefined
 where
  cbcEncHelper :: Block -> Key -> [Block] -> [Block]
  cbcEncHelper _         _   []     = []
  cbcEncHelper prevBlock key blocks = result
    : cbcEncHelper result key (tail blocks)
    where result = encrypt key (head blocks `bsXor` prevBlock)

decryptStream
  :: ModeOfOperation
  -> InitializationVector
  -> Key
  -> BlockStream
  -> BlockStream
decryptStream modeOfOperation iv key blocks
  | modeOfOperation == ECB
  = B.concat . map (decrypt (padKeyIV key)) . splitEvery 16 . pad $ blocks
  | modeOfOperation == CBC
  = undefined
  | modeOfOperation == CTR
  = undefined

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

decrypt :: Key -> Block -> Block
decrypt key = helper (reverse $ genSubKeys key)
 where
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

toByte :: Char -> W.Word8
toByte = fromIntegral . ord

pad :: BlockStream -> BlockStream
pad blocks = blocks `B.append` B.replicate (16 - (B.length blocks `mod` 16)) 0

padKeyIV :: B.ByteString -> Key
padKeyIV key = key `B.append` B.replicate (16 - B.length key) 0
  where len = B.length key

testAES :: Key -> Block -> IO ()
testAES key block = printBS block >> printBS encrypted >> printBS
  (decrypt key encrypted)
  where encrypted = encrypt key block

