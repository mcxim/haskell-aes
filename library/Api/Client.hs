{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Api.Client where

import           Data.Aeson                     ( object
                                                , withObject
                                                , ToJSON
                                                , FromJSON
                                                , (.:)
                                                , (.=)
                                                , encode
                                                , decode
                                                , parseJSON
                                                , toJSON
                                                )
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Servant.Client
import           Servant.API
import qualified System.IO                     as SIO
import qualified Encryption.Utils              as EU
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.UTF8     as BLU
import qualified Data.ByteString.Base64.Lazy   as B64
-- import qualified Debug.Trace                   as DT
import qualified Encryption.EncDec             as ED
import qualified Encryption.Globals            as EG


run :: ClientM a -> IO (Either ClientError a)
run query = do
  manager' <- newManager defaultManagerSettings
  runClientM query (mkClientEnv manager' (BaseUrl Http "localhost" 5000 ""))

type Username = String
type Userdata = String -- b64 encoded

data User = User {_username :: Username, _userdata :: Userdata} deriving (Show, Generic)

instance ToJSON User where
  toJSON (User un ud) = object ["username" .= un, "data" .= ud]

data UserSchema = UserSchema {__username :: Username, __userdata :: Userdata, __id :: Int}
  deriving (Show, Generic)

instance FromJSON UserSchema where
  parseJSON = withObject "UserSchema" $ \obj -> do
    un  <- obj .: "username"
    ud  <- obj .: "data"
    id' <- obj .: "id"
    return (UserSchema un ud id')

newtype TokenInJSON = TokenInJSON String deriving (Show, Generic)

instance FromJSON TokenInJSON where
  parseJSON = withObject "TokenInJSON" $ \obj -> do
    t <- obj .: "token"
    return (TokenInJSON t)

data Credentials = Credentials String String

instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \obj -> do
    username <- obj .: "username"
    password <- obj .: "password"
    return (Credentials username password)

instance ToJSON Credentials where
  toJSON (Credentials username password) =
    object ["username" .= username, "password" .= password]

type Token = String

type API = "register" :> ReqBody '[JSON] Credentials :> PostCreated '[JSON] UserSchema
      :<|> "login" :>  Header "Authorization" String :> Get '[JSON] TokenInJSON
      :<|> "user" :> (
                               Header "x-access-tokens" Token :>                                  Get '[JSON] UserSchema
                :<|> "data" :> Header "x-access-tokens" Token :>                                  Get '[PlainText] String
                :<|> "data" :> Header "x-access-tokens" Token :> ReqBody '[PlainText] Userdata :> Put '[PlainText] String
                :<|>           Header "x-access-tokens" Token :>                                  DeleteNoContent '[PlainText] NoContent
           ) 

api :: Proxy API
api = Proxy

registerHandler :: Credentials -> ClientM UserSchema
loginHandler :: Maybe Token -> ClientM TokenInJSON
getAllHandler :: Maybe Token -> ClientM UserSchema
getVaultHandler :: Maybe Token -> ClientM String
putVaultHandler :: Maybe Token -> Userdata -> ClientM String
deleteUserHandler :: Maybe Token -> ClientM NoContent
registerHandler :<|> loginHandler :<|> (getAllHandler :<|> getVaultHandler :<|> putVaultHandler :<|> deleteUserHandler)
  = client api

showError :: Either ClientError a -> Either String a
showError (Left  err) = Left $ show err
showError (Right val) = Right val

register :: Credentials -> IO (Either String UserSchema)
register = fmap showError . run . registerHandler

login :: String -> String -> IO (Either String TokenInJSON)
login username password =
  fmap showError
    .  run
    .  loginHandler
    .  Just
    .  ("Basic " <>)
    .  BLU.toString
    .  B64.encode
    .  BLU.fromString
    $  username
    <> ":"
    <> password

getAll :: Token -> IO (Either String UserSchema)
getAll = fmap showError . run . getAllHandler . Just

getVault :: Token -> IO (Either String String)
getVault = fmap showError . run . getVaultHandler . Just

putVault :: Token -> Userdata -> IO (Either String String)
putVault token = fmap showError . run . putVaultHandler (Just token)

deleteUser = fmap showError . run . deleteUserHandler . Just


data Entry = Entry {site :: String, name :: String, pass :: String}
  deriving (Show, Generic)

instance ToJSON Entry

instance FromJSON Entry

decryptData
  :: EG.Key -> EG.InitializationVector -> String -> Either String [Entry]
decryptData key iv data' = do
  encrypted <- B64.decode (BLU.fromString data')
  let (check, rest) =
        B.splitAt 5 $ ED.decryptStream EG.CBC iv key EG.KS256 encrypted
  if check /= B.pack [99, 104, 101, 99, 107]
    then Left "Error: Decryption failed."
    else case decode rest of
      Nothing      -> Left "Error: JSON decoding error."
      Just entries -> Right entries

getData
  :: Token -> EG.Key -> EG.InitializationVector -> IO (Either String [Entry])
getData token key iv = (>>= decryptData key iv) <$> getVault token

encryptData :: EG.Key -> EG.InitializationVector -> [Entry] -> String
encryptData key iv =
  BLU.toString
    . B64.encode
    . ED.encryptStream EG.CBC iv key EG.KS256
    . B.append (B.pack [99, 104, 101, 99, 107])
    . encode

putData :: Token -> EG.Key -> EG.InitializationVector -> [Entry] -> IO (Either String String)
putData token key iv entriesToAdd = getData token key iv >>= \case
  Left err -> return $ Left err
  Right entries -> putVault token (encryptData key iv (entries <> entriesToAdd))

-- addEntries
--   :: Int
--   -> EG.Key
--   -> EG.InitializationVector
--   -> [Entry]
--   -> IO (Either String UserSchema)
-- addEntries id' key iv entriesToAdd = getDataById 1 key iv >>= \case
--   Left err -> return $ Left err
--   Right entries ->
--     putUser id' (User " " (encryptData key iv (entries ++ entriesToAdd)))


-- changeName :: Int -> String -> IO (Either String UserSchema)
-- changeName id' newName = putUser id' (User newName " ")


testB64 :: IO ()
testB64 = do
  fhandle  <- SIO.openFile "result.txt" SIO.ReadMode
  contents <- B.hGetContents fhandle
  EU.printBS contents
  print contents
  let encoded = B64.encode contents
  EU.printBS encoded
  print encoded
  let decoded = B64.decode encoded
  print decoded
  return ()
