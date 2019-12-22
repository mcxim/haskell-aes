module Encryption.MixColumns
  ( mixColumns
  , invMixColumns
  )
where

import           Encryption.Utils
import           Encryption.Globals
import qualified Data.ByteString               as B
import qualified Data.Word8                    as W
import           Crypto.Number.F2m              ( mulF2m
                                                , addF2m
                                                )
import           Data.List                      ( foldl' )

-- Works.

mixColumns :: Block -> Block
mixColumns = common matrix

invMixColumns :: Block -> Block
invMixColumns = common invMatrix

common :: [B.ByteString] -> Block -> Block
common someMatrix =
  B.concat . map (mulMV aesPolynomial someMatrix) . splitEvery 4

mulMV :: Integer -> [B.ByteString] -> B.ByteString -> B.ByteString
mulMV polynomial matrix vector =
  B.pack
    . map
        ( fromInteger
        . foldl' addF2m 0
        . B.zipWith (mulF2mBytes polynomial) vector
        )
    $ matrix

mulF2mBytes :: Integer -> W.Word8 -> W.Word8 -> Integer
mulF2mBytes polynomial w1 w2 =
  mulF2m polynomial (fromIntegral w1) (fromIntegral w2)

matrix = map
  B.pack
  [ [0x02, 0x03, 0x01, 0x01]
  , [0x01, 0x02, 0x03, 0x01]
  , [0x01, 0x01, 0x02, 0x03]
  , [0x03, 0x01, 0x01, 0x02]
  ]

invMatrix = map
  B.pack
  [ [0x0E, 0x0B, 0x0D, 0x09]
  , [0x09, 0x0E, 0x0B, 0x0D]
  , [0x0D, 0x09, 0x0E, 0x0B]
  , [0x0B, 0x0D, 0x09, 0x0E]
  ]
