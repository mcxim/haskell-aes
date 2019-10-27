module EncDec (encrypt, decrypt) where

import ShiftRows
import AddRoundKey
import MixColumns
import SBox
import Utils
import Globals


encrypt = undefined
decrypt = undefined

encRound :: Key -> Block -> Block
encRound key = addRoundKey key . mixColumns . shiftRows . subBytes