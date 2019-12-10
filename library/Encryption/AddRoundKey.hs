module AddRoundKey
  ( addRoundKey
  )
where

import           Globals
import           Utils

addRoundKey :: Key -> Block -> Block
addRoundKey = bsXor
