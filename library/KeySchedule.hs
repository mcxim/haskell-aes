module KeySchedule where

import qualified Data.Word8                    as W
import           Data.Bits
import qualified Data.ByteString               as B
import           Globals
import           Utils
import           SBox

genSubKeys :: Key -> [B.ByteString]
genSubKeys key = tail $ helper roundCoefficients [key]

helper :: [B.ByteString] -> [B.ByteString] -> [B.ByteString]
helper []  keys         = keys
helper rcs computedKeys = helper (tail rcs)
                                 (computedKeys ++ [newSubKeyConcatenated])
 where
  (pw1 : pw2 : pw3 : pw4 : shouldBeNothing) = splitEvery 4 $ last computedKeys
  newSubKey :: [B.ByteString]
  newSubKey =
    [ rotWordLeft 1 (subBytes pw4) `bsxor` pw1 `bsxor` head rcs
    , head newSubKey `bsxor` pw2
    , newSubKey !! 1 `bsxor` pw3
    , newSubKey !! 2 `bsxor` pw4
    ]
  newSubKeyConcatenated = B.concat newSubKey

roundCoefficients :: [B.ByteString]
roundCoefficients =
  [ B.pack [0x01, 0, 0, 0]
  , B.pack [0x02, 0, 0, 0]
  , B.pack [0x04, 0, 0, 0]
  , B.pack [0x08, 0, 0, 0]
  , B.pack [0x10, 0, 0, 0]
  , B.pack [0x20, 0, 0, 0]
  , B.pack [0x40, 0, 0, 0]
  , B.pack [0x80, 0, 0, 0]
  , B.pack [0x1B, 0, 0, 0]
  , B.pack [0x36, 0, 0, 0]
  ]
