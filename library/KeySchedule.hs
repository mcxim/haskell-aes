module KeySchedule
  ( genSubKeys
  )
where

-- Works.

import qualified Data.ByteString               as B
import           Globals
import           Utils
import           SBox

genSubKeys :: Key -> [SubKey]
genSubKeys key = helper roundCoefficients [key]

helper :: [RoundCoefficient] -> [Key] -> [Key]
helper []  keys         = keys
helper rcs computedKeys = helper (tail rcs)
                                 (computedKeys `snoc` newSubKeyConcatenated)
 where
  (pw1 : pw2 : pw3 : pw4 : _) = splitEvery 4 $ last computedKeys
  newSubKey =
    [ rotWordLeft 1 (subBytes pw4) `bsXor` pw1 `bsXor` head rcs
    , head newSubKey `bsXor` pw2
    , newSubKey !! 1 `bsXor` pw3
    , newSubKey !! 2 `bsXor` pw4
    ] :: [B.ByteString]
  newSubKeyConcatenated = B.concat newSubKey

roundCoefficients :: [RoundCoefficient]
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
