{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Api.Client where

import qualified Data.Text                     as T
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
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.UTF8     as BLU
import qualified Data.ByteString.Base64.Lazy   as B64
import qualified Data.Maybe                    as DM
import qualified Debug.Trace                   as DT
import           Encryption.EncDec


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

getUsers = run getUsers'
getUserById = run . getUserById'
postUser = run . postUser'
putUser = run . putUser'
deleteUser = run . deleteUser'


data Entry = Entry {site :: String, name :: String, pass :: String}
  deriving (Show, Generic)

instance ToJSON Entry

instance FromJSON Entry

type Password = String

getDataById :: Int -> Password -> IO (Either String [Entry])
getDataById id password =
  getUserById id
    >>= (\case
          Left err -> return . Left $ show err
          Right (UserWId _ data' _) ->
            case B64.decode (BLU.fromString data') of
              Left  err     -> return . Left $ err
              Right encoded -> case decode encoded of
                Nothing      -> return . Left $ "Error: decoding error."
                Just entries -> return . Right $ entries
        )

-- DM.fromMaybe "Error: decoding error." (decode encoded :: [Entry])
  -- B64.decode userdata' $ getUserById id




-- test :: IO ()
-- test = do
--   fhandle  <- SIO.openFile "key.txt" SIO.ReadMode
--   contents <- B.hGetContents fhandle
--   EU.printBS contents
--   print contents
--   let encoded = B64.encode contents
--   EU.printBS encoded
--   print encoded
--   let decoded = B64.decode encoded
--   print decoded
--   return ()
