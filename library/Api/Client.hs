{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Api.Client where

import           Data.Aeson
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
import qualified Debug.Trace                   as DT
import qualified Encryption.EncDec             as ED
import qualified Encryption.Globals            as EG


run :: ClientM a -> IO (Either ClientError a)
run query = do
  manager' <- newManager defaultManagerSettings
  runClientM query (mkClientEnv manager' (BaseUrl Http "localhost" 5000 ""))

type Username = String
type Userdata = String -- b64 encoded

data User = User {getUsername :: Username, getData :: Userdata} deriving (Show, Generic)

instance ToJSON User where
  toJSON (User un ud) = object ["username" .= un, "data" .= ud]

data UserWId = UserWId {getUsername' :: Username, getData' :: Userdata, getId :: Int}
  deriving (Show, Generic)

instance FromJSON UserWId where
  parseJSON = withObject "UserWId" $ \obj -> do
    un  <- obj .: "username"
    ud  <- obj .: "data"
    id' <- obj .: "id"
    return (UserWId un ud id')

type API = "user" :> (
            Get '[JSON] [UserWId]
      :<|>  Capture "userid" Int :> Get '[JSON] UserWId
      :<|>  ReqBody '[JSON] User :> Post '[JSON] UserWId
      :<|>  ReqBody '[JSON] User :> PutAccepted '[JSON] UserWId
      :<|>  Capture "userid" Int :> DeleteAccepted '[JSON] UserWId
      )

api :: Proxy API
api = Proxy

getUserById' :: Int -> ClientM UserWId
getUsers' :: ClientM [UserWId]
postUser' :: User -> ClientM UserWId
putUser' :: User -> ClientM UserWId
deleteUser' :: Int -> ClientM UserWId
getUsers' :<|> getUserById' :<|> postUser' :<|> putUser' :<|> deleteUser' =
  client api

getUsers :: IO (Either ClientError [UserWId])
getUsers = run getUsers'
getUserById :: Int -> IO (Either ClientError UserWId)
getUserById = run . getUserById'
postUser :: User -> IO (Either ClientError UserWId)
postUser = run . postUser'
putUser :: User -> IO (Either ClientError UserWId)
putUser = run . putUser'
deleteUser :: Int -> IO (Either ClientError UserWId)
deleteUser = run . deleteUser'


data Entry = Entry {site :: String, name :: String, pass :: String}
  deriving (Show, Generic)

instance ToJSON Entry

instance FromJSON Entry

getDataById
  :: Int -> EG.Key -> EG.InitializationVector -> IO (Either String [Entry])
getDataById id' key iv = do
  user <- getUserById id'
  return $ case user of
    Left err -> Left $ show err
    Right (UserWId _ encodedData _) ->
      let encrypted = B64.decode (BLU.fromString encodedData)
      in  case encrypted of
            Left err -> Left err
            Right encoded ->
              let decrypted = ED.decryptStream EG.CBC iv key EG.KS256 encoded
              in  case decode decrypted of
                    Nothing      -> Left "Error: decoding error."
                    Just entries -> Right entries

encryptData :: [Entry] -> EG.Key -> EG.InitializationVector -> B.ByteString
encryptData = undefined

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
