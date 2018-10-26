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

newtype Weight = Weight Int
               deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype AccessToken = AccessToken Text
                    deriving (Eq, Ord, Show, ToHttpApiData, IsString)

newtype MilestoneId = MilestoneId Int
                    deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype ProjectId = ProjectId Int
                  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype Labels = Labels (S.Set Text)
               deriving (Semigroup, Monoid, Show)

data StatusEvent = CloseEvent | ReopenEvent
                 deriving (Show)

instance ToJSON StatusEvent where
    toJSON CloseEvent  = "close"
    toJSON ReopenEvent = "reopen"

mkLabel :: Text -> Labels
mkLabel = Labels . S.singleton

instance IsString Labels where
    fromString = mkLabel . T.pack

instance ToJSON Labels where
    toJSON (Labels lbls) = toJSON $ T.intercalate "," (S.toList lbls)

newtype IssueIid = IssueIid Int
                 deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

type GitLabRoot = Header "Private-Token" AccessToken

----------------------------------------------------------------------
-- getIssue
----------------------------------------------------------------------

type GetIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> Post '[JSON] IssueResp

getIssue :: AccessToken -> ProjectId -> IssueIid -> ClientM IssueResp
getIssue = client (Proxy :: Proxy GetIssueAPI) . Just


data IssueResp
    = IssueResp { irProjectId :: ProjectId
                , irIid :: IssueIid
                }
    deriving (Show)

instance FromJSON IssueResp where
    parseJSON = withObject "issue response" $ \o ->
      IssueResp <$> o .: "project_id"
                <*> o .: "iid"

----------------------------------------------------------------------
-- createIssue
----------------------------------------------------------------------

data CreateIssue
    = CreateIssue { ciIid :: Maybe IssueIid
                  , ciTitle :: Text
                  , ciLabels :: Maybe Labels
                  , ciCreatedAt :: Maybe UTCTime
                  , ciDescription :: Maybe Text
                  , ciMilestoneId :: Maybe (Maybe MilestoneId)
                  , ciWeight :: Maybe Weight
                  }

instance ToJSON CreateIssue where
    toJSON CreateIssue{..} = object
        [ "iid" .= ciIid
        , "title" .= ciTitle
        , "labels" .= ciLabels
        , "created_at" .= ciCreatedAt
        , "description" .= ciDescription
        , "milestone_id" .= ciMilestoneId
        , "weight" .= ciWeight
        ]

type CreateIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> ReqBody '[JSON] CreateIssue
    :> Post '[JSON] IssueResp

createIssue :: AccessToken -> ProjectId -> CreateIssue -> ClientM IssueResp
createIssue = client (Proxy :: Proxy CreateIssueAPI) . Just

----------------------------------------------------------------------
-- editIssue
----------------------------------------------------------------------

data EditIssue
    = EditIssue { eiTitle       :: Maybe Text
                , eiDescription :: Maybe Text
                , eiMilestoneId :: Maybe (Maybe MilestoneId)
                , eiLabels      :: Maybe Labels
                , eiStatus      :: Maybe StatusEvent
                , eiUpdateTime  :: Maybe UTCTime
                , eiWeight      :: Maybe Weight
                }
    deriving (Show)

instance ToJSON EditIssue where
    toJSON EditIssue{..} = object
        [ "title" .= eiTitle
        , "description" .= eiDescription
        , "milestone_id" .= eiMilestoneId
        , "labels" .= eiLabels
        , "status" .= eiStatus
        , "updated_at" .= eiUpdateTime
        , "weight" .= eiWeight
        ]


type EditIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> ReqBody '[JSON] EditIssue
    :> Put '[JSON] IssueResp

editIssue :: AccessToken -> ProjectId -> IssueIid -> EditIssue -> ClientM IssueResp
editIssue = client (Proxy :: Proxy EditIssueAPI) . Just

----------------------------------------------------------------------
-- createIssueNote
----------------------------------------------------------------------

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

----------------------------------------------------------------------
-- deleteIssue
----------------------------------------------------------------------

type DeleteIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> Delete '[] NoContent

deleteIssue :: AccessToken -> ProjectId -> IssueIid -> ClientM ()
deleteIssue tok prj iid = void $ client (Proxy :: Proxy DeleteIssueAPI) (Just tok) prj iid
