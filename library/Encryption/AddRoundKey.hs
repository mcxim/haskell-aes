module Encryption.AddRoundKey
  ( addRoundKey
  )
where

import           Encryption.Globals
import           Encryption.Utils

addRoundKey :: Key -> Block -> Block
addRoundKey = bsXor
