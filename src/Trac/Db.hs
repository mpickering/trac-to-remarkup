{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Trac.Db where

import Data.Functor.Identity
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Trac.Db.Types
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ


deriving instance FromField TicketNumber
deriving instance ToField TicketNumber

newtype TracTime = TracTime UTCTime
                 deriving (Eq, Ord, Show)

instance FromField TracTime where
    fromField field bs = do
        t <- fromField field bs :: Conversion Integer
        return $ TracTime $ posixSecondsToUTCTime $ realToFrac t / 1000000

-- N.B. Work around tuple width limit
type Row =
    (Integer, Text, TracTime, Text,
     Text, Text, Text) :.
    (Maybe Text, Text, Maybe Text,
     Maybe Text, Maybe Text, TracTime)

getTickets :: Connection -> IO [Ticket]
getTickets conn = do
    mapM toTicket =<< query_ conn
      [sql|SELECT id, type, time, component,
                  priority, reporter, status,
                  version, summary, milestone,
                  keywords, description, changetime
           FROM ticket
          |]
  where
    findOrigField :: FromField a => Text -> TicketNumber -> IO (Maybe a)
    findOrigField field (TicketNumber n) = do
        mval <- query conn [sql|SELECT oldvalue
                                FROM ticket_change
                                WHERE ticket = ?
                                AND field = ?
                                ORDER BY time ASC
                                LIMIT 1
                               |]
                      (n, field)
        return $ case mval of
          [] -> Nothing
          [Only x] -> x

    toTicket :: Row -> IO Ticket
    toTicket ((n, typ, TracTime ticketCreationTime, component,
               prio, reporter, status) :.
              (mb_version, summary, mb_milestone,
               mb_keywords, mb_description, TracTime ticketChangeTime))
      = do
        let ticketStatus = Identity New
            ticketNumber = TicketNumber n
            ticketCreator = reporter

        let findOrig :: FromField a => Text -> a -> IO a
            findOrig field def = fromMaybe def <$> findOrigField field ticketNumber

            parseTicketList :: T.Text -> [TicketNumber]
            parseTicketList = mapMaybe parseTicketNumber . T.words

            parseTicketNumber :: T.Text -> Maybe TicketNumber
            parseTicketNumber =
                either (const Nothing) (Just . TicketNumber . fst) .
                TR.decimal . T.dropWhile (=='#') . T.strip

            parseDifferentials :: T.Text -> [Differential]
            parseDifferentials = const [] -- TODO

            i = Identity
        ticketSummary <- i <$> findOrig "summary" summary
        ticketComponent <- i <$> findOrig "component" component

        ticketType <- i . toTicketType <$> findOrig "type" typ
        ticketPriority <- i . toPriority <$> findOrig "priority" prio
        ticketVersion <- i <$> findOrig "version" (fromMaybe "" mb_version)
        ticketMilestone <- i <$> findOrig "milestone" (fromMaybe "" mb_milestone)
        ticketKeywords <- i . T.words <$> findOrig "keywords" (fromMaybe "" mb_keywords)
        ticketBlockedBy <- i . maybe [] parseTicketList <$> findOrigField "blockedby" ticketNumber
        ticketRelated <- i . maybe [] parseTicketList <$> findOrigField "related" ticketNumber
        ticketBlocking <- i . maybe [] parseTicketList <$> findOrigField "blocking" ticketNumber
        ticketDifferentials <- i . maybe [] parseDifferentials <$> findOrigField "differential" ticketNumber
        ticketTestCase <- i . fromMaybe "" <$> findOrigField "testcase" ticketNumber
        ticketDescription <- i <$> findOrig "description" (fromMaybe "" mb_description)
        ticketTypeOfFailure <- i . toTypeOfFailure <$> findOrig "failure" ""
        let ticketFields = Fields {..}
        return Ticket {..}

getTicketChanges :: Connection -> TicketNumber -> IO [TicketChange]
getTicketChanges conn n = do
    map toChange <$> query conn
      [sql|SELECT time, author, field, newvalue
           FROM ticket_change
           WHERE ticket = ?
           ORDER BY time ASC
          |]
      (Only n)
  where
    toChange :: (TracTime, Text, Text, Maybe Text) -> TicketChange
    toChange (TracTime t, author, field, new) =
        case field of
          "type"        -> fieldChange $ emptyFields{ticketType = Just $ toTicketType $ expectJust new}
          "summary"     -> fieldChange $ emptyFields{ticketSummary = Just $ expectJust new}
          "description" -> fieldChange $ emptyFields{ticketDescription = new}
          "priority"    -> fieldChange $ emptyFields{ticketPriority = Just $ toPriority $ expectJust new}
          "status"      -> fieldChange $ emptyFields{ticketStatus = Just $ toStatus $ expectJust new}

          "comment"     -> empty {changeComment = Just $ expectJust new}
          _             -> empty
      where
        expectJust Nothing = error $ unlines [ "expected Just newvalue:"
                                             , "  t: " <> show t
                                             , "  field: " <> show field
                                             , "  newvalue: "<>  show new
                                             ]
        expectJust (Just x) = x

        empty = TicketChange { changeTime = t
                             , changeAuthor = author
                             , changeFields = emptyFields
                             , changeComment = Nothing
                             }
        fieldChange flds = empty {changeFields = flds}

toStatus :: Text -> Status
toStatus t = case t of
    "new"        -> New
    "assigned"   -> Assigned
    "patch"      -> Patch
    "merge"      -> Merge
    "closed"     -> Closed
    "infoneeded" -> InfoNeeded
    "upstream"   -> Upstream
    "reopened"   -> New
    _            -> error $ "unknown status: " ++ show t

toPriority :: Text -> Priority
toPriority t = case t of
    "lowest"  -> PrioLowest
    "low"     -> PrioLow
    "normal"  -> PrioNormal
    "high"    -> PrioHigh
    "highest" -> PrioHighest
    _ -> PrioNormal

toTicketType :: Text -> TicketType
toTicketType t = case t of
    "bug"  -> Bug
    "task" -> Task
    "merge" -> MergeReq
    "feature request" -> FeatureRequest
    _ -> Bug -- TODO

toTypeOfFailure :: Text -> TypeOfFailure
toTypeOfFailure t = case t of
    "Building GHC failed" -> BuildingGhcFailed
    "Compile-time crash" -> CompileTimeCrash
    "Compile-time crash or panic" -> CompileTimeCrash
    "Compile-time performance bug" -> CompileTimePerformance
    "Debugging information is incorrect" -> IncorrectDebugInformation
    "Documentation bug" -> DocumentationBug
    "GHC accepts invalid program" -> InvalidProgramAccepted
    "GHC doesn't work at all" -> GhcDoesn'tWork
    "GHCi crash" -> GhciCrash
    "GHC rejects valid program" -> ValidProgramRejected
    "Incorrect API annotation" -> IncorrectAPIAnnotation
    "Incorrect error/warning at compile-time" -> IncorrectWarning
    "Incorrect result at runtime" -> IncorrectResultAtRuntime
    "Incorrect warning at compile-time" -> IncorrectWarning
    "Installing GHC failed" -> InstallationFailure
    "None/Unknown" -> OtherFailure
    "Other" -> OtherFailure
    "Poor/confusing error message" -> PoorErrorMessage
    "Runtime crash" -> RuntimeCrash
    "Runtime performance bug" -> RuntimePerformance
    "" -> OtherFailure

data Milestone = Milestone { mName :: Text
                           , mDescription :: Text
                           , mDue :: UTCTime
                           , mCompleted :: UTCTime
                           }

getMilestones :: Connection -> IO [Milestone]
getMilestones conn = do
    map f <$> query_ conn
      [sql|SELECT name, due, completed, description
           FROM milestone
          |]
  where
    f (mName, TracTime mDue, TracTime mCompleted, mDescription) =
        Milestone {..}

getAttachments :: Connection -> IO [Attachment]
getAttachments conn = do
    map f <$> query_ conn
        [sql|SELECT type, id, filename, time, description, author, ipnr
             FROM attachment |]
  where
    f :: (Text, Text, Text, TracTime, Text, Text, Text) -> Attachment
    f (typ, rid, aFilename, TracTime aTime, aDescription, aAuthor, aIpAddr) =
        Attachment {..}
      where
        aResource = case typ of
          "ticket" -> TicketAttachment $ TicketNumber $ read $ T.unpack rid
          "wiki" -> WikiAttachment $ WikiName rid
