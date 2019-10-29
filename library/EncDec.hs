module EncDec where

import           ShiftRows
import           AddRoundKey
import           MixColumns
import           SBox
import           Globals
import           Utils
import           KeySchedule
import qualified Data.ByteString               as B
import qualified Data.Word8                    as W
import           Data.Char                      ( ord )

encryptStream :: ModeOfOperation -> InitVector -> Key -> [Block] -> [Block]
encryptStream ECB _  key blocks = map (encrypt key) blocks
encryptStream CBC iv key blocks = undefined
encryptStream CTR iv key blocks = undefined

decryptStream :: ModeOfOperation -> InitVector -> Key -> [Block] -> [Block]
decryptStream ECB _  key blocks = map (decrypt key) blocks
decryptStream CBC iv key blocks = undefined
decryptStream CTR iv key blocks = undefined

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

toBlocks :: String -> BlockString
toBlocks = undefined

pad :: String -> Block
pad string = B.pack . map toByte

toByte :: Char -> W.Word8
toByte = fromIntegral . ord

testAES :: Key -> Block -> IO ()
testAES key block = printBS block >> printBS encrypted >> printBS
  (decrypt key encrypted)
  where encrypted = encrypt key block

