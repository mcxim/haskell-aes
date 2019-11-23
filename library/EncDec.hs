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
import           Debug.Trace                    ( trace )

encryptStream
  :: ModeOfOperation
  -> InitializationVector
  -> Key
  -> KeySize
  -> BlockStream
  -> BlockStream
encryptStream modeOfOperation iv key keySize blocks
  | modeOfOperation == ECB
  = B.concat . map (encrypt subKeys keySize) . splitEvery 16 . padPkcs7 $ blocks
  | modeOfOperation == CBC
  = B.concat $ cbcEncHelper (padIV iv) (splitEvery 16 (padPkcs7 blocks))
  | modeOfOperation == CTR
  = undefined
  | otherwise
  = undefined
 where
  subKeys = genSubKeys (padKey key keySize) keySize
  cbcEncHelper :: Block -> [Block] -> [Block]
  cbcEncHelper _         []               = []
  cbcEncHelper prevBlock (block : blocks) = result : cbcEncHelper result blocks
    where result = encrypt subKeys keySize (block `bsXor` prevBlock)

decryptStream
  :: ModeOfOperation
  -> InitializationVector
  -> Key
  -> KeySize
  -> BlockStream
  -> BlockStream
decryptStream modeOfOperation iv key keySize blocks
  | modeOfOperation == ECB
  = B.concat
    . unpadPkcs7
    . map (decrypt subKeys keySize)
    . splitEvery 16
    $ blocks
  | modeOfOperation == CBC
  = B.concat . unpadPkcs7 $ cbcDecHelper (padIV iv) (splitEvery 16 blocks)
  | modeOfOperation == CTR
  = undefined
  | otherwise
  = undefined
 where
  subKeys = reverse $ genSubKeys (padKey key keySize) keySize
  cbcDecHelper :: Block -> [Block] -> [Block]
  cbcDecHelper prevCipherText [block] =
    pure $ decrypt subKeys keySize block `bsXor` prevCipherText
  cbcDecHelper prevCipherText (block : blocks) =
    decrypt subKeys keySize block
      `bsXor` prevCipherText
      :       cbcDecHelper block blocks
  cbcDecHelper _ _ = undefined

  -- | trace ("Current state: " ++ reprBS block ++ "\nSubkeys Left: " ++ show (length subKeys)) False = undefined
encrypt :: [SubKey] -> KeySize -> Block -> Block
encrypt subKeys keySize block
  | length subKeys == 1
  = addRoundKey (head subKeys) . shiftRows . subBytes $ block
  | length subKeys == numRounds keySize + 1
  = encrypt (tail subKeys) keySize . addRoundKey (head subKeys) $ block
  | otherwise
  = encrypt (tail subKeys) keySize
    . addRoundKey (head subKeys)
    . mixColumns
    . shiftRows
    . subBytes
    $ block

-- SHOULD GET SUBKEYS IN REVERSED ORDER!
decrypt :: [SubKey] -> KeySize -> Block -> Block
decrypt subKeys keySize
  | length subKeys == 1
  = addRoundKey (head subKeys) . invSubBytes . invShiftRows
  | length subKeys == numRounds keySize + 1
  = decrypt (tail subKeys) keySize . addRoundKey (head subKeys)
  | otherwise
  = decrypt (tail subKeys) keySize
    . invMixColumns
    . addRoundKey (head subKeys)
    . invSubBytes
    . invShiftRows

toByte :: Char -> W.Word8
toByte = fromIntegral . ord

pad :: BlockStream -> BlockStream -- deprecated
pad blocks = blocks
  `B.append` B.replicate (if modulo == 0 then 0 else 16 - modulo) 0
  where modulo = B.length blocks `mod` 16

unpad :: [Block] -> [Block] -- deprecated
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

padKey :: B.ByteString -> KeySize -> Key
padKey key keySize =
  key `B.append` B.replicate (getKeySize keySize - B.length key) 0

padIV :: B.ByteString -> InitializationVector
padIV iv = iv `B.append` B.replicate (16 - B.length iv) 0

testBS = B.pack [1]

testBlock = do
  let ks = KS128
  testKS ks
  putStrLn "Original block: "
  printBS sampleBlock
  let subKeys = genSubKeys (padKey sampleKey ks) ks
  putStrLn "SubKeys: "
  printBSL subKeys
  let encryptedBlock = encrypt subKeys ks sampleBlock
  putStrLn "Encrypted block: "
  printBS encryptedBlock
  let decryptedBlock = decrypt (reverse subKeys) ks encryptedBlock
  putStrLn "Decrypted block: "
  printBS decryptedBlock
