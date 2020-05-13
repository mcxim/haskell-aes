module Encryption.AddRoundKey
  ( addRoundKey
  )
where

import           Encryption.Globals
import           Encryption.Utils

-- Round key layer of AES (which is a xor operation).
addRoundKey :: Key -> Block -> Block
addRoundKey = bsXor
