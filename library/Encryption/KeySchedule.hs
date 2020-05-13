module Encryption.KeySchedule
  ( genSubKeys
  )
where


import qualified Data.ByteString.Lazy          as B
import           Encryption.Globals
import           Encryption.Utils
import           Encryption.SBox

-- Generate subkeys from Key given key size.
genSubKeys :: Key -> KeySize -> [SubKey]
genSubKeys key KS128 =
  splitEvery 16 $ helper128 (take 10 roundCoefficients) key
genSubKeys key KS192 =
  splitEvery 16 $ helper192 (take 08 roundCoefficients) key
genSubKeys key KS256 =
  splitEvery 16 $ helper256 (take 07 roundCoefficients) key

-- Helper for generating subkeys of length 128 bits.
helper128 :: [RoundCoefficient] -> B.ByteString -> B.ByteString
helper128 []  keys         = keys
helper128 rcs computedKeys = helper128
  (tail rcs)
  (computedKeys `B.append` B.concat newSubKey)
 where
  (pw1 : pw2 : pw3 : pw4 : _) =
    splitEvery 4 . B.reverse . B.take 16 . B.reverse $ computedKeys
  newSubKey =
    [ rotWordLeft 1 (subBytes pw4) `bsXor` pw1 `bsXor` head rcs
    , head newSubKey `bsXor` pw2
    , newSubKey !! 1 `bsXor` pw3
    , newSubKey !! 2 `bsXor` pw4
    ] :: [B.ByteString]

-- Helper for generating subkeys of length 192 bits.
helper192 :: [RoundCoefficient] -> B.ByteString -> B.ByteString
helper192 []  keys         = keys
helper192 rcs computedKeys = helper192
  (tail rcs)
  (computedKeys `B.append` B.concat newSubKey)
 where
  lenRcs = length rcs
  (pw1 : pw2 : pw3 : pw4 : pw5 : pw6 : _) =
    splitEvery 4 . B.reverse . B.take 24 . B.reverse $ computedKeys
  newSubKey = take
    (if lenRcs == 1 then 4 else 6)
    [ rotWordLeft 1 (subBytes pw6) `bsXor` pw1 `bsXor` head rcs
    , head newSubKey `bsXor` pw2
    , newSubKey !! 1 `bsXor` pw3
    , newSubKey !! 2 `bsXor` pw4
    , newSubKey !! 3 `bsXor` pw5
    , newSubKey !! 4 `bsXor` pw6
    ]

-- Helper for generating subkeys of length 256 bits.
helper256 :: [RoundCoefficient] -> B.ByteString -> B.ByteString
helper256 []  keys         = keys
helper256 rcs computedKeys = helper256
  (tail rcs)
  (computedKeys `B.append` B.concat newSubKey)
 where
  lenRcs = length rcs
  (pw1 : pw2 : pw3 : pw4 : pw5 : pw6 : pw7 : pw8 : _) =
    splitEvery 4 . B.reverse . B.take 32 . B.reverse $ computedKeys
  newSubKey = take
    (if lenRcs == 1 then 4 else 8)
    [ rotWordLeft 1 (subBytes pw8) `bsXor` pw1 `bsXor` head rcs
    , head newSubKey `bsXor` pw2
    , newSubKey !! 1 `bsXor` pw3
    , newSubKey !! 2 `bsXor` pw4
    , subBytes (newSubKey !! 3) `bsXor` pw5
    , newSubKey !! 4 `bsXor` pw6
    , newSubKey !! 5 `bsXor` pw7
    , newSubKey !! 6 `bsXor` pw8
    ]

-- Constant AES round coefficients.
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
