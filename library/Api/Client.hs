{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Api.Client where

import qualified Data.Aeson                    as J
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

data User = User {getUsername :: Username, getData :: Userdata} deriving (Show, Generic)

instance J.ToJSON User where
  toJSON (User un ud) = J.object ["username" J..= un, "data" J..= ud]

data UserWId = UserWId {getUsername' :: Username, getData' :: Userdata, getId :: Int}
  deriving (Show, Generic)

instance J.FromJSON UserWId where
  parseJSON = J.withObject "UserWId" $ \obj -> do
    un  <- obj J..: "username"
    ud  <- obj J..: "data"
    id' <- obj J..: "id"
    return (UserWId un ud id')

type API = "user" :> (
            Get '[JSON] [UserWId]
      :<|>  Capture "userid" Int :> Get '[JSON] UserWId
      :<|>  ReqBody '[JSON] User :> Post '[JSON] UserWId
      :<|>  Capture "userid" Int :> ReqBody '[JSON] User :> PutAccepted '[JSON] UserWId
      :<|>  Capture "userid" Int :> DeleteAccepted '[JSON] UserWId
      )

api :: Proxy API
api = Proxy

getUsers' :: ClientM [UserWId]
getUserById' :: Int -> ClientM UserWId
postUser' :: User -> ClientM UserWId
putUser' :: Int -> User -> ClientM UserWId
deleteUser' :: Int -> ClientM UserWId
getUsers' :<|> getUserById' :<|> postUser' :<|> putUser' :<|> deleteUser' =
  client api

showError :: Either ClientError a -> Either String a
showError (Left  err) = Left $ show err
showError (Right val) = Right val

getUsers :: IO (Either String [UserWId])
getUsers = showError <$> run getUsers'
getUserById :: Int -> IO (Either String UserWId)
getUserById id' = showError <$> run (getUserById' id')
postUser :: User -> IO (Either String UserWId)
postUser user = showError <$> run (postUser' user)
putUser :: Int -> User -> IO (Either String UserWId)
putUser i u = showError <$> run (putUser' i u)
deleteUser :: Int -> IO (Either String UserWId)
deleteUser id' = showError <$> run (deleteUser' id')


data Entry = Entry {site :: String, name :: String, pass :: String}
  deriving (Show, Generic)

instance J.ToJSON Entry

instance J.FromJSON Entry

decryptData
  :: EG.Key -> EG.InitializationVector -> String -> Either String [Entry]
decryptData key iv data' = do
  encrypted <- B64.decode (BLU.fromString data')
  let (check, rest) =
        B.splitAt 5 $ ED.decryptStream EG.CBC iv key EG.KS256 encrypted
  if check /= B.pack [99, 104, 101, 99, 107]
    then Left "Error: Decryption failed."
    else case J.decode rest of
      Nothing      -> Left "Error: JSON decoding error."
      Just entries -> Right entries

getDataById
  :: Int -> EG.Key -> EG.InitializationVector -> IO (Either String [Entry])
getDataById id' key iv =
  (>>= decryptData key iv) . fmap getData' <$> getUserById id'

encryptData :: EG.Key -> EG.InitializationVector -> [Entry] -> String
encryptData key iv =
  BLU.toString
    . B64.encode
    . ED.encryptStream EG.CBC iv key EG.KS256
    . B.append (B.pack [99, 104, 101, 99, 107])
    . J.encode

addEntries
  :: Int
  -> EG.Key
  -> EG.InitializationVector
  -> [Entry]
  -> IO (Either String UserWId)
addEntries id' key iv entriesToAdd = getDataById 1 key iv >>= \case
  Left err -> return $ Left err
  Right entries ->
    putUser id' (User " " (encryptData key iv (entries ++ entriesToAdd)))


changeName :: Int -> String -> IO (Either String UserWId)
changeName id' newName = putUser id' (User newName " ")


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
