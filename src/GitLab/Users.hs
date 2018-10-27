{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Users where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Semigroup
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Proxy
import Data.String
import Data.Time.Clock
import Servant.API
import Servant.Client
import GitLab.Common

----------------------------------------------------------------------
-- createUser
----------------------------------------------------------------------

data CreateUser
    = CreateUser { cuEmail       :: Text
                 , cuUsername    :: Text
                 , cuName        :: Text
                 , cuSkipConfirmation :: Bool
                 }
    deriving (Show)

instance ToJSON CreateUser where
    toJSON CreateUser{..} = object
        [ "email" .= cuEmail
        , "username" .=  cuUsername
        , "name" .=  cuName
        , "reset_password" .= True
        , "skip_confirmation" .= cuSkipConfirmation
        ]

data CreateUserResp = CreateUserResp UserId

instance FromJSON CreateUserResp where
    parseJSON = withObject "create user response" $ \o -> do
        CreateUserResp <$> o .: "id"

type CreateUserAPI =
    GitLabRoot :> "users"
    :> ReqBody '[JSON] CreateUser
    :> Post '[JSON] CreateUserResp

createUser :: AccessToken -> CreateUser -> ClientM UserId
createUser tok cu = do
    CreateUserResp uid <- client (Proxy :: Proxy CreateUserAPI) (Just tok) cu
    return uid


----------------------------------------------------------------------
-- findUserByUsername
----------------------------------------------------------------------

type FindUserByUsernameAPI =
    GitLabRoot :> "users"
    :> QueryParam "username" Text
    :> Get '[JSON] [CreateUserResp]

findUserByUsername :: AccessToken -> Text -> ClientM (Maybe UserId)
findUserByUsername tok username = do
    res <- client (Proxy :: Proxy FindUserByUsernameAPI) (Just tok) (Just username)
    return $ case res of
               [] -> Nothing
               [CreateUserResp uid] -> Just uid
               _ -> error $ "Multiple users with id "<>show username

----------------------------------------------------------------------
-- findUserByEmail
----------------------------------------------------------------------

type FindUserByEmailAPI =
    GitLabRoot :> "users"
    :> QueryParam "email" Text
    :> Get '[JSON] [CreateUserResp]

findUserByEmail :: AccessToken -> Text -> ClientM (Maybe UserId)
findUserByEmail tok email = do
    res <- client (Proxy :: Proxy FindUserByEmailAPI) (Just tok) (Just email)
    return $ case res of
               [] -> Nothing
               [CreateUserResp uid] -> Just uid
               _ -> error $ "Multiple users with email "<>show email

----------------------------------------------------------------------
-- addProjectMember
----------------------------------------------------------------------

type AddProjectMemberAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "members"
    :> ReqBody '[JSON] AddProjectMember
    :> Post '[JSON] Object

data AccessLevel = Guest | Reporter | Developer | Maintainer | Owner

instance ToJSON AccessLevel where
    toJSON l = toJSON $ case l of
                          Guest      -> 10 :: Int
                          Reporter   -> 20
                          Developer  -> 30
                          Maintainer -> 40
                          Owner      -> 50

data AddProjectMember = AddProjectMember UserId AccessLevel

instance ToJSON AddProjectMember where
    toJSON (AddProjectMember uid access) = object
        [ "user_id" .= uid
        , "access_level" .= access
        ]

addProjectMember :: AccessToken -> ProjectId
                 -> UserId -> AccessLevel -> ClientM ()
addProjectMember tok prj uid access = do
    client (Proxy :: Proxy AddProjectMemberAPI) (Just tok) prj
        $ AddProjectMember uid access
    return ()
