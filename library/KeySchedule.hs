module KeySchedule
  ( genSubKeys
  )
where

import           Data.Word8
import           Data.Bits
import qualified Data.ByteString               as B
import           Globals
import           Utils
import           SBox

genSubKeys :: Key -> [B.ByteString]
genSubKeys key = tail $ helper (numRounds + 1) [key]

helper :: Int -> [B.ByteString] -> [B.ByteString]
helper 0          keys         = keys
helper roundsLeft computedKeys = helper (roundsLeft - 1)
                                        (computedKeys ++ [newSubKey])
 where
  [pw1, pw2, pw3, pw4] = splitEvery 4 $ last computedKeys
  newSubKey = B.concat
    [ rotWordLeft 1 . subBytes $ pw4 `xor` pw1 `xor` undefined
    , head newSubKey `xor` pw2
    , newSubKey !! 1 `xor` pw3
    , newSubKey !! 2 `xor` pw4
    ]

-- g :: B.ByteString -> Byte -> B.ByteString
-- g bs roundConstant = (xor y roundConstant `B.cons` xs) `B.snoc` x
--  where
--   x  = B.head bs
--   y  = B.head . B.tail $ bs
--   xs = B.tail . B.tail $ bs
