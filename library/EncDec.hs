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
import           Debug.Trace                    ( trace )

encryptStream
  :: ModeOfOperation
  -> InitializationVector
  -> Key
  -> BlockStream
  -> BlockStream
encryptStream modeOfOperation iv key blocks
  | modeOfOperation == ECB
  = B.concat . map (encrypt (padKeyIV key)) . splitEvery 16 . padPkcs7 $ blocks
  | modeOfOperation == CBC
  = B.concat $ cbcEncHelper (padKeyIV iv)
                            (padKeyIV key)
                            (splitEvery 16 (padPkcs7 blocks))
 where
  cbcEncHelper :: Block -> Key -> [Block] -> [Block]
  cbcEncHelper _ _ [] = []
  cbcEncHelper prevBlock key (block : blocks) =
    result : cbcEncHelper result key blocks
    where result = encrypt key (block `bsXor` prevBlock)

decryptStream
  :: ModeOfOperation
  -> InitializationVector
  -> Key
  -> BlockStream
  -> BlockStream
decryptStream modeOfOperation iv key blocks
  | modeOfOperation == ECB
  = B.concat
    . unpadPkcs7
    . map (decrypt (padKeyIV key))
    . splitEvery 16
    . pad
    $ blocks
  | modeOfOperation == CBC
  = B.concat . unpadPkcs7 $ cbcDecHelper (padKeyIV iv)
                                         (padKeyIV key)
                                         (splitEvery 16 blocks)
  | modeOfOperation == CTR
  = undefined
 where
  cbcDecHelper :: Block -> Key -> [Block] -> [Block]
  cbcDecHelper prevCipherText key [block] =
    pure $ decrypt key block `bsXor` prevCipherText
  cbcDecHelper prevCipherText key (block : blocks) =
    decrypt key block `bsXor` prevCipherText : cbcDecHelper block key blocks

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
pad blocks = blocks
  `B.append` B.replicate (if modulo == 0 then 0 else 16 - modulo) 0
  where modulo = B.length blocks `mod` 16

unpad :: [Block] -> [Block]
unpad blocks =
  init blocks ++ [B.reverse . B.dropWhile (== 0) . B.reverse . last $ blocks]

padPkcs7 :: BlockStream -> BlockStream
padPkcs7 blocks = blocks `B.append` B.replicate padNum (fromIntegral padNum)
  where padNum = 16 - (B.length blocks `mod` 16)

unpadPkcs7 :: [Block] -> [Block]
unpadPkcs7 blocks =
  xs ++ [B.reverse . B.dropWhile (== fromIntegral padLength) . B.reverse $ x]
 where
  xs        = init blocks
  x         = last blocks
  padLength = B.last x

padKeyIV :: B.ByteString -> Key
padKeyIV key = key `B.append` B.replicate (16 - B.length key) 0
  where len = B.length key

testAES :: Key -> Block -> IO ()
testAES key block = printBS block >> printBS encrypted >> printBS
  (decrypt key encrypted)
  where encrypted = encrypt key block

testBS = B.pack [1]
