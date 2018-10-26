{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Tickets where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Semigroup
import Data.Aeson
import Data.Proxy
import Data.String
import Data.Time.Clock
import Servant.API
import Servant.Client

newtype AccessToken = AccessToken Text
                    deriving (Eq, Ord, Show, ToHttpApiData, IsString)

newtype ProjectId = ProjectId Int
                  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype Labels = Labels (S.Set Text)
               deriving (Semigroup, Monoid, Show)

mkLabel :: Text -> Labels
mkLabel = Labels . S.singleton

instance IsString Labels where
    fromString = mkLabel . T.pack

instance ToJSON Labels where
    toJSON (Labels lbls) = toJSON $ T.intercalate "," (S.toList lbls)

newtype IssueIid = IssueIid Int
                 deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

data IssueResp
    = IssueResp { irProjectId :: ProjectId
                , irIid :: IssueIid
                }
    deriving (Show)

instance FromJSON IssueResp where
    parseJSON = withObject "issue response" $ \o ->
      IssueResp <$> o .: "project_id"
                <*> o .: "iid"

data CreateIssue
    = CreateIssue { ciIid :: Maybe IssueIid
                  , ciTitle :: Text
                  , ciLabels :: Maybe Labels
                  , ciCreatedAt :: Maybe UTCTime
                  , ciDescription :: Maybe Text
                  }

instance ToJSON CreateIssue where
    toJSON CreateIssue{..} = object
        [ "iid" .= ciIid
        , "title" .= ciTitle
        , "labels" .= ciLabels
        , "created_at" .= ciCreatedAt
        , "description" .= ciDescription
        ]

type GitLabRoot = Header "Private-Token" AccessToken

type CreateIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> ReqBody '[JSON] CreateIssue
    :> Post '[JSON] IssueResp

createIssue :: AccessToken -> ProjectId -> CreateIssue -> ClientM IssueResp
createIssue = client (Proxy :: Proxy CreateIssueAPI) . Just

data CreateIssueNote
    = CreateIssueNote { cinBody :: Text
                      , cinCreatedAt :: Maybe UTCTime
                      }

instance ToJSON CreateIssueNote where
    toJSON CreateIssueNote{..} = object
        [ "body" .= cinBody
        , "created_at" .= cinCreatedAt
        ]

data IssueNoteResp
    = IssueNoteResp { inrId :: Int
                    }
    deriving (Show)

instance FromJSON IssueNoteResp where
    parseJSON = withObject "issue note response" $ \o ->
      IssueNoteResp <$> o .: "id"

type CreateIssueNoteAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid :> "notes"
    :> ReqBody '[JSON] CreateIssueNote
    :> Post '[JSON] IssueNoteResp

createIssueNote :: AccessToken -> ProjectId -> IssueIid -> CreateIssueNote -> ClientM IssueNoteResp
createIssueNote = client (Proxy :: Proxy CreateIssueNoteAPI) . Just

type DeleteIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> Delete '[] NoContent

deleteIssue :: AccessToken -> ProjectId -> IssueIid -> ClientM ()
deleteIssue tok prj iid = void $ client (Proxy :: Proxy DeleteIssueAPI) (Just tok) prj iid
