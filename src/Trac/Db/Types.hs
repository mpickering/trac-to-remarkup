{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Trac.Db.Types where

import Control.Applicative
import Data.Functor.Identity
import Data.Text (Text)
import Data.Time.Clock

newtype TracTime = TracTime UTCTime
                 deriving (Eq, Ord, Show)

type RawTime = Integer

newtype TicketNumber = TicketNumber { getTicketNumber :: Integer }
                     deriving (Show, Read, Ord, Eq)

data TicketType = FeatureRequest | Bug | MergeReq | Task
                deriving (Show)

data Ticket = Ticket { ticketNumber       :: TicketNumber
                     , ticketCreationTime :: UTCTime
                     , ticketChangeTime   :: UTCTime
                     , ticketFields       :: Fields Identity
                     , ticketCreator      :: Text
                     }
            deriving (Show)

data MutationType = CreateTicket -- ^ Ticket creation
                  | ChangeTicket -- ^ One or more changes
            deriving (Show, Read, Enum, Ord, Eq)

-- | Ticket mutation; this data structure does not, however, give us
-- the complete ticket mutation information, only the ticket number,
-- modification timestamp, and whether it was a ticket creation or
-- a set of ticket changes. Multiple ticket changes with the same timestamp
-- and ticket number are reported as one mutation.
data TicketMutation = TicketMutation { ticketMutationTicket :: TicketNumber
                                     , ticketMutationTime :: RawTime
                                     , ticketMutationType :: MutationType
                                     }
                                     deriving (Show, Read, Ord, Eq)

data Priority = PrioLowest | PrioLow | PrioNormal | PrioHigh | PrioHighest
              deriving (Show)

data Status = New | Assigned | Patch | Merge | Closed | InfoNeeded | Upstream
            deriving (Show)

data Fields f = Fields { ticketType          :: f TicketType
                       , ticketSummary       :: f Text
                       , ticketComponent     :: f Text
                       , ticketPriority      :: f Priority
                       , ticketVersion       :: f Text
                       , ticketMilestone     :: f Text
                       , ticketDescription   :: f Text
                       , ticketTypeOfFailure :: f TypeOfFailure
                       , ticketKeywords      :: f [Text]
                       , ticketBlockedBy     :: f [TicketNumber]
                       , ticketRelated       :: f [TicketNumber]
                       , ticketBlocking      :: f [TicketNumber]
                       , ticketDifferentials :: f [Differential]
                       , ticketTestCase      :: f Text
                       , ticketStatus        :: f Status
                       }

hoistFields :: (forall a. f a -> g a) -> Fields f -> Fields g
hoistFields f Fields{..} =
    Fields { ticketType          = f ticketType
           , ticketSummary       = f ticketSummary
           , ticketComponent     = f ticketComponent
           , ticketPriority      = f ticketPriority
           , ticketVersion       = f ticketVersion
           , ticketMilestone     = f ticketMilestone
           , ticketDescription   = f ticketDescription
           , ticketTypeOfFailure = f ticketTypeOfFailure
           , ticketKeywords      = f ticketKeywords
           , ticketBlockedBy     = f ticketBlockedBy
           , ticketRelated       = f ticketRelated
           , ticketBlocking      = f ticketBlocking
           , ticketDifferentials = f ticketDifferentials
           , ticketTestCase      = f ticketTestCase
           , ticketStatus        = f ticketStatus
           }

emptyFields :: Fields Maybe
emptyFields = Fields
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing

collapseFields :: Fields Maybe -> Fields Maybe -> Fields Maybe
collapseFields a b =
    Fields { ticketType = ticketType a <|> ticketType b
           , ticketSummary = ticketSummary a <|> ticketSummary b
           , ticketComponent = ticketComponent a <|> ticketComponent b
           , ticketPriority = ticketPriority a <|> ticketPriority b
           , ticketVersion = ticketVersion a <|> ticketVersion b
           , ticketMilestone = ticketMilestone a <|> ticketMilestone b
           , ticketDescription = ticketDescription a <|> ticketDescription b
           , ticketTypeOfFailure = ticketTypeOfFailure a <|> ticketTypeOfFailure b
           , ticketKeywords = ticketKeywords a <|> ticketKeywords b
           , ticketBlockedBy = ticketBlockedBy a <|> ticketBlockedBy b
           , ticketRelated = ticketRelated a <|> ticketRelated b
           , ticketBlocking = ticketBlocking a <|> ticketBlocking b
           , ticketDifferentials = ticketDifferentials a <|> ticketDifferentials b
           , ticketTestCase = ticketTestCase a <|> ticketTestCase b
           , ticketStatus = ticketStatus a <|> ticketStatus b
           }

deriving instance Show (Fields Identity)
deriving instance Show (Fields Maybe)

newtype Differential = Differential Integer
                     deriving (Show)

data TicketChange = TicketChange { changeTime    :: UTCTime
                                 , changeAuthor  :: Text
                                 , changeFields  :: Fields Maybe
                                 , changeComment :: Maybe Text
                                 }

                  deriving (Show)

data TypeOfFailure
    = BuildingGhcFailed
    | CompileTimeCrash
    | CompileTimePerformance
    | IncorrectDebugInformation
    | DocumentationBug
    | InvalidProgramAccepted
    | GhcDoesn'tWork
    | GhciCrash
    | ValidProgramRejected
    | IncorrectAPIAnnotation
    | PoorErrorMessage
    | IncorrectWarning
    | IncorrectResultAtRuntime
    | InstallationFailure
    | RuntimeCrash
    | RuntimePerformance
    | OtherFailure
    deriving (Show)

newtype WikiName = WikiName Text
                 deriving (Ord, Eq, Show, Read)

data Attachment = Attachment { aResource    :: AttachmentResource
                             , aFilename    :: Text
                             , aTime        :: UTCTime
                             , aDescription :: Text
                             , aAuthor      :: Text
                             , aIpAddr      :: Maybe Text
                             }
                deriving (Show, Read)

data AttachmentResource = TicketAttachment !TicketNumber
                        | WikiAttachment !WikiName
                        deriving (Ord, Eq, Show, Read)
