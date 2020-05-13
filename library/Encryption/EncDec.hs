module Encryption.EncDec where

import           Encryption.ShiftRows
import           Encryption.AddRoundKey
import           Encryption.MixColumns
import           Encryption.SBox
import           Encryption.Globals
import           Encryption.Utils
import           Encryption.KeySchedule
import qualified Data.ByteString.Lazy          as B
import qualified Data.Word8                    as W
import           Data.Char                      ( ord )
import           Crypto.Classes.Exceptions      ( newGenIO )
import           Crypto.Random                  ( genBytes
                                                , SystemRandom
                                                )

-- AES-encrypt list of blocks.
encryptStream -- The function encryptStream is defined as
  :: ModeOfOperation -- a function that takes: a mode of operation,
  -> InitializationVector -- an initialization vector,
  -> Key -- a key,
  -> KeySize -- a key size,
  -> BlockStream -- and a stream of blocks
  -> BlockStream -- and returns a stream of blocks.
encryptStream modeOfOperation iv key keySize
  -- When the mode is ECB, pad the stream, split it every 16 blocks,
  -- encrypt each block with the subkeys and concatenate the results
  -- back into a continuous stream.
  | modeOfOperation == ECB
  = B.concat . map (encrypt subKeys keySize) . splitEvery 16 . padPkcs7
  -- When the mode is CBC, use the CBC encryption helper function.
  | modeOfOperation == CBC
  = B.concat . (pure (padIV iv) <>) . cbcEncHelper (padIV iv) . splitEvery 16 . padPkcs7
 where
  -- Define the subkeys by calling the key schedule:
  subKeys = genSubKeys (padKey key keySize) keySize
  -- The function cbcEncHelper is defined as a function that takes a
  -- block and a list of blocks and returns a list of blocks. The
  -- function recursivelly takes care of the block chaining in CBC.
  cbcEncHelper :: Block -> [Block] -> [Block]
  cbcEncHelper _         []               = []
  cbcEncHelper prevBlock (block : blocks) = result : cbcEncHelper result blocks
    where result = encrypt subKeys keySize (block `bsXor` prevBlock)

-- Generate a random IV for CBC encryption.
encryptRandomCBC :: Key -> KeySize -> BlockStream -> IO BlockStream
encryptRandomCBC key keySize blockstream = do
  g  <- newGenIO :: IO SystemRandom
  iv <- case genBytes 16 g of
    Left  err          -> error $ show err
    Right (result, g2) -> return result
  return $ encryptStream CBC (B.fromStrict iv) key keySize blockstream

-- Decrypt AES-encrypted list of blocks.
decryptStream -- TODO get iv from ct
  :: ModeOfOperation -> Key -> KeySize -> BlockStream -> BlockStream
decryptStream modeOfOperation key keySize blockstream
  | modeOfOperation == ECB
  = B.concat . unpadPkcs7 . map (decrypt subKeys keySize) . splitEvery 16 $ blockstream
  | modeOfOperation == CBC
  = let (iv : blockstream') = splitEvery 16 blockstream
    in  B.concat . unpadPkcs7 . cbcDecHelper (padIV iv) $ blockstream'
  | otherwise
  = undefined
 where
  subKeys = genSubKeys (padKey key keySize) keySize
  cbcDecHelper :: Block -> [Block] -> [Block]
  cbcDecHelper prevCipherText [block] =
    pure $ decrypt subKeys keySize block `bsXor` prevCipherText
  cbcDecHelper prevCipherText (block : blocks) =
    decrypt subKeys keySize block `bsXor` prevCipherText : cbcDecHelper block blocks
  cbcDecHelper arg1 arg2 = error (show arg1 ++ ", " ++ show arg2)

-- Encrypt a single block with AES.
encrypt :: [SubKey] -> KeySize -> Block -> Block
encrypt subKeys keySize
  | length subKeys == 1
  = addRoundKey (head subKeys) . shiftRows . subBytes
  | length subKeys == numRounds keySize + 1
  = encrypt (tail subKeys) keySize . addRoundKey (head subKeys)
  | otherwise
  = encrypt (tail subKeys) keySize
    . addRoundKey (head subKeys)
    . mixColumns
    . shiftRows
    . subBytes

-- Decrypt a single block with AES.
decrypt :: [SubKey] -> KeySize -> Block -> Block
decrypt subKeys keySize
  | length subKeys == 1
  = addRoundKey (last subKeys) . invSubBytes . invShiftRows
  | length subKeys == numRounds keySize + 1
  = decrypt (init subKeys) keySize . addRoundKey (last subKeys)
  | otherwise
  = decrypt (init subKeys) keySize
    . invMixColumns
    . addRoundKey (last subKeys)
    . invSubBytes
    . invShiftRows

-- Pad list of blocks with PKCS7 padding.
padPkcs7 :: BlockStream -> BlockStream
padPkcs7 blocks = blocks `B.append` B.replicate padNum (fromIntegral padNum)
  where padNum = let n = 16 - (B.length blocks `mod` 16) in if n == 0 then 16 else n

-- Unpad list of blocks with PKCS7 padding.
unpadPkcs7 :: [Block] -> [Block]
unpadPkcs7 blocks =
  xs ++ [B.reverse . B.dropWhile (== fromIntegral padLength) . B.reverse $ x]
 where
  xs        = init blocks
  x         = last blocks
  padLength = B.last x

-- Pad key (fill empty space with zeros).
padKey :: B.ByteString -> KeySize -> Key
padKey key keySize = key `B.append` B.replicate (getKeySize keySize - B.length key) 0

-- Pad iv (fill empty space with zeros).
padIV :: B.ByteString -> InitializationVector
padIV iv = iv `B.append` B.replicate (16 - B.length iv) 0